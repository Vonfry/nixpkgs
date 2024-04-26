;;; update-elpa --- -*- lexical-binding: t -*-

;;; Commentary:
;; This is the updater for recipes-archive-elpa.json and other servers with
;; compatible elpa.


;;; Code:

(require 'package)
(require 'url-http)
(require 'json)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

;; # Updater

;; ## Structures

(cl-defstruct updater-args
  (name   nil :type string)
  (server nil :type string)
  (output nil :type string))

(cl-defstruct fetch-info
  "The infomation extracted from recipe and archive."
  (name nil :type symbol)
  (desc nil :type package-desc)
  (hash nil :type string))

(cl-defstruct elpa-archive
  "Elpa original contents and previous recipes in nix."
  contents previous)

;; ## CLI Args Parser
(defun parse-cli-args ()
  "Parse cli arguments to `updater-args' for `run-updater'."
  (make-updater-args
   :name   (pop command-line-args-left)
   :server (pop command-line-args-left)
   :output (pop command-line-args-left)))

;; ## Previous Archive Reader
(defun parse-previous-archive (filename)
  "Parse previous generated recipes in FILENAME by this script."
  (let ((idx (make-hash-table :test 'equal)))
    (cl-loop for desc in
          (let ((json-object-type 'hash-table)
                (json-array-type 'list)
                (json-key-type 'symbol))
            (when (file-exists-p filename)
              (json-read-file filename)))
          do (puthash (gethash 'ename desc)
                      desc idx))
    idx))

;; ## Prefetcher

(defcustom *prefetch-max-concurrence* 64
  "The max concurrence threads for prefetch.

This is to avoid opening file count restriction."
  :type 'integer
  :group 'nix-emacs-updater)

(defvar *prefetch-current-concurrence* 0
  "The current concurrence threads count for prefetch.

This is to avoid opening file count restriction with
`*prefetch-max-concurrence*'.")

(cl-defmethod fetch-info-url ((info fetch-info))
  "Get the fetch url from INFO.

The url generation copies from `package-install-from-archive' and
`package--with-response-buffer-1'."
  (let* ((pkg-desc (fetch-info-desc info))
         (base (package-archive-base pkg-desc))
         (file (concat (package-desc-full-name pkg-desc)
                       (package-desc-suffix pkg-desc))))
    (url-expand-file-name file base)))

(cl-defmethod get-prefetch-cmd ((info fetch-info))
  "Get the prefetch command into a list.

INFO is the preftech info for the download one."
  (let ((url (fetch-info-url info)))
    (list "nix-prefetch-url" url)))

(defun to-sri (hash)
  "Convert hexadeciamal HASH to SRI."
  (with-temp-buffer
    (call-process "nix-hash"
                  nil
                  t
                  t
                  "--to-sri"
                  "--type"
                  "sha256"
                  hash)
    (goto-char (point-min))
    (buffer-substring (pos-bol) (pos-eol))))

(defvar *prefetch-info* nil
  "A dynamic binding var used in callback after prefetch.

It should be `prefitch-info'")

(defun update-fetch-info-hash (var)
  "Update the hash in `*prefetch-fetch-info*' to VAR."
  (when (fetch-info-p *prefetch-info*)
    (setf (fetch-info-hash *prefetch-info*) var)
    (with-slots (name version) (fetch-info-desc *prefetch-info*)
      (message "INFO: %s: update %s %s"
               name version var))))

(defun process-prefetch-output (proc output)
  "Process the OUTPUT from prefetching PROC and save into fetch-info.

The fetch-info should be saved in `*prefetch-info*' as buffer local."
  (with-current-buffer (process-buffer proc)
    (setf *prefetch-current-concurrence*
          (- *prefetch-current-concurrence* 1))
    (when (fetch-info-p *prefetch-info*)
      (update-fetch-info-hash (to-sri (car (split-string output "\n")))))))

(defun prefetch-async ()
  "Prefetch REPO at COMMIT with FETCHER to get the hash.

The fetch-info should be saved in `*prefetch-fetch-info*'."
  (when (fetch-info-p *prefetch-info*)
    (let* ((cmd (get-prefetch-cmd *prefetch-info*))
           (prog (car cmd)))
      (while (>= *prefetch-current-concurrence* *prefetch-max-concurrence*)
        ; restrict max concurrence count to avoid reaching max opening file
        ; count in linux.
        (sleep-for 5))
      (with-current-buffer (generate-new-buffer (concat "*" prog "*"))
        (make-local-variable '*prefetch-info*)
        (setf *prefetch-current-concurrence*
              (+ 1 *prefetch-current-concurrence*))
        (make-process
         :name prog
         :buffer (current-buffer)
         :filter #'process-prefetch-output
         :stderr (messages-buffer)
         :noquery t
         :connection-type nil
         :command cmd)))))

(defun source-hash-async (previous)
  "Get the hash according to packages infomation.

The fetch-info should be saved in `*prefetch-info*'.

PREVIOUS: previous generated recipes data."
  (when (fetch-info-p *prefetch-info*)
    (with-slots (desc) *prefetch-info*
      (with-slots (name version) desc
        (let* ((prev-version (when previous (gethash 'version previous)))
               (prev-hash    (when previous (gethash 'hash previous))))
          (if (and version prev-hash (equal prev-version version))
              (prog2 (message "INFO: %s: re-using %s %s"
                              name prev-version prev-hash)
                  (update-fetch-info-hash prev-hash))
            (prefetch-async)))))))

(cl-defmethod make-recipe-nix ((info fetch-info))
  "Make a final recipe data used in nix from INFO."
  (with-slots (name desc hash) info
    (with-slots (extras version reqs summary) desc
        `((ename . ,name)
          (version . ,(mapconcat #'prin1-to-string version "."))
          (url . ,(fetch-info-url info))
          (hash . ,hash)
          (deps . ,(sort (remove 'emacs (mapcar 'car reqs)) 'string<))
          (homepage . ,(alist-get :url extras))
          (description . ,summary)))))

(defun start-fetch (archives)
  "Start fetch all ARCHIVES and MELPA-ARCHIVE information such as hash."
  (let (recipes-info recipes-nix)
    (setf recipes-info
          (mapcar
           (lambda (archive)
             (let* ((name (car archive))
                    (desc (cadr archive))

                    (previous (when-let ((previous (elpa-archive-previous
                                                    archives)))
                                (gethash name previous)))
                    info)
               (setf info (make-fetch-info :name name :desc desc))

               (let ((*prefetch-info* info))
                 (source-hash-async previous))
               info))
           (elpa-archive-contents archives)))

    (while-let ((info (pop recipes-info)))
      (with-slots (hash) info
        (if hash
            (push (make-recipe-nix info) recipes-nix)
          (progn
            (sleep-for 1)
            (push info recipes-info)))))

    recipes-nix))

;; ## Emitter

(cl-defmethod emit-json ((archives elpa-archive))
  "Generate the recipes for nix in json with ARCHIVES from melpa."
  (when-let (descriptors (start-fetch archives))
    (message "Finished downloading %d descriptors" (length descriptors))
    (let ((buf (generate-new-buffer "*recipes-archive*")))
      (with-current-buffer buf
        (insert (json-encode descriptors))
        buf))))

(defun get-elpa-archive ()
  "Get elpa archive with NAME on SERVER."
  (let (package-archive-contents)
    (package-refresh-contents)
    (package-read-all-archive-contents)
    package-archive-contents))

;; ## Main runner

(defun run-updater (&optional args)
  "Run elpa updater to generate a recipe file in json read by nix.

ARGS: some configuration of the updater.  It is read from cli arguments,
unless typed as `updater-args'."
  (message "Turning off logging to *Message* buffer")

  (unless (updater-args-p args)
    (message "Parsing args from cli.")
    (setf args (parse-cli-args)))

  (let ((message-log-max nil)
        (process-adaptive-read-buffering nil)
        (archives (make-elpa-archive))
        recipes-nix-buf
        (package-archives (list (cons (updater-args-name args)
                                      (updater-args-server args)))))

    (setf (elpa-archive-contents archives) (get-elpa-archive))
    (setf (elpa-archive-previous archives)
          (parse-previous-archive (updater-args-output args)))

    (setf recipes-nix-buf (emit-json archives))

    (with-current-buffer recipes-nix-buf
      (write-file (updater-args-output args)))))

(provide 'update-elpa)

;;; update-elpa.el ends here
