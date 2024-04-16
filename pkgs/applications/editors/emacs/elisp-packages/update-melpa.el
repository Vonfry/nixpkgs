;;; update-melpa --- -*- lexical-binding: t -*-

;;; Commentary:
;; This is the updater for recipes-archive-melpa.json

;;; Code:

(require 'url-http)
(require 'json)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

;; # Updater

;; ## Structures

(cl-defstruct fetch-info
  "The infomation extracted from recipe and archive."
  ename
  fetcher
  url
  repo
  stable-archive
  stable-hash
  unstable-archive
  unstable-hash)

(cl-defstruct prefetch-info
  "The infomation for prefetching."
  (info nil :type fetch-info)
  variant)

(cl-defstruct melpa-archive
  "Melpa original recipes, archive and previous recipes in nix."
  recipes
  stable
  unstable
  previous)

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

(defvar *prefetch-max-concurrence* 64
  "The max concurrence threads count for prefetch.

This is to avoid opening file count restriction.")

(defvar *prefetch-current-concurrence* 0
  "The current concurrence threads count for prefetch.

This is to avoid opening file count restriction with
`*prefetch-max-concurrence*'.")

(defun get-prefetch-cmd (fetcher repo commit)
  "Get the prefetch command into a list.

The first element is the program to run, and others are arguments.

FETCHER is the type of source.
REPO is the package name.
COMMIT is the version of the REPO."
  (pcase fetcher
    ("github" (list "nix-prefetch-url"
                    "--unpack"
                    (concat "https://github.com/"
                            repo
                            "/archive/"
                            commit
                            ".tar.gz")))
    ("gitlab" (list "nix-prefetch-url"
                    "--unpack"
                    (concat "https://gitlab.com/api/v4/projects/"
                            (url-hexify-string repo)
                            "/repository/archive.tar.gz?ref="
                            commit)))
    ("sourcehut" (list "nix-prefetch-url"
                       "--unpack"
                       (concat "https://git.sr.ht/~"
                               repo
                               "/archive/"
                               commit
                               ".tar.gz")))
    ("codeberg" (list "nix-prefetch-url"
                      "--unpack"
                      (concat "https://codeberg.org/"
                              repo
                              "/archive/"
                              commit
                              ".tar.gz")))
    ("bitbucket" (list "nix-prefetch-hg"
                       (concat "https://bitbucket.com/" repo)
                       commit))
    ("hg" (list "nix-prefetch-hg"
                repo
                commit))
    ("git" (list "nix-prefetch-git"
                 "--fetch-submodules"
                 "--url" repo
                 "--rev" commit))
    (_ (throw 'unknown-fetcher fetcher))))

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

(cl-defmethod prefetch-info-archive ((info prefetch-info))
  "Get the archive from INFO according to its variant."
  (with-slots (info variant) info
    (pcase variant
      ('unstable (fetch-info-unstable-archive info))
      ('stable (fetch-info-stable-archive info)))))

(defun update-fetch-info-hash (var)
  "Update the hash in `*prefetch-fetch-info*' to VAR."
  (when (prefetch-info-p *prefetch-info*)
    (with-slots (info variant) *prefetch-info*
      (pcase variant
        ('unstable (setf (fetch-info-unstable-hash info) var))
        ('stable (setf (fetch-info-stable-hash info) var)))
      (with-slots (ename commit) info
        (let* ((archive (prefetch-info-archive *prefetch-info*))
               (commit (gethash 'commit (gethash 'props archive))))
          (message "INFO: %s: update repository %s %s"
                   ename commit var))))))

(defun process-prefetch-output (proc output)
  "Process the OUTPUT from prefetching PROC and save into fetch-info.

The fetch-info should be saved in `*prefetch-info*' as buffer local."
  (with-current-buffer (process-buffer proc)
    (setf *prefetch-current-concurrence*
          (- *prefetch-current-concurrence* 1))
    (when (prefetch-info-p *prefetch-info*)
      (with-slots (info) *prefetch-info*
        (with-slots (fetcher) info
          (update-fetch-info-hash
           (pcase fetcher
             ("git" (alist-get 'hash (json-parse-string output)))
             (_ (to-sri (car (split-string output "\n")))))))))))

(defun prefetch-async ()
  "Prefetch REPO at COMMIT with FETCHER to get the hash.

The fetch-info should be saved in `*prefetch-fetch-info*'."
  (when (prefetch-info-p *prefetch-info*)
    (with-slots (info) *prefetch-info*
      (with-slots (fetcher repo) info
        (let* ((archive (prefetch-info-archive *prefetch-info*))
               (commit (gethash 'commit (gethash 'props archive)))
               (cmd (get-prefetch-cmd fetcher repo commit))
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
         :command cmd)))))))

(defun source-hash-async (previous)
  "Get the hash according to packages infomation.

The fetch-info should be saved in `*prefetch-info*'.

PREVIOUS: previous generated recipes data."
  (when (prefetch-info-p *prefetch-info*)
    (with-slots (info) *prefetch-info*
      (with-slots (ename repo url) info
        (let* ((archive (prefetch-info-archive *prefetch-info*))
               (commit (gethash 'commit (gethash 'props archive)))
               (prev-commit (when previous (gethash 'commit previous)))
               (prev-hash (when previous (gethash 'hash previous))))
          (cond ((and commit prev-hash (equal prev-commit commit))
                 (message "INFO: %s: re-using %s %s"
                          ename prev-commit prev-hash)
                 (update-fetch-info-hash prev-hash))
                ((and commit (or repo url))
                 (prefetch-async))
                (t (message "ERROR: %s: no commit information" ename)
                   (update-fetch-info-hash 'missing))))))))


(defun source-info (aentry source-hash)
  "Generate the source infomation.

AENTRY: The stable or unstable content of the recipe.
SOURCE-HASH: The hash of source."
  (let* ((version (and aentry (gethash 'ver aentry)))
         (deps    (when-let (deps (gethash 'deps aentry))
                    (remove 'emacs (hash-table-keys deps))))
         (aprops  (and aentry (gethash 'props aentry)))
         (commit  (gethash 'commit aprops)))
    (append `((version . ,(string-replace
                           ".-" "-"
                           (mapconcat #'prin1-to-string version ".")))
              (commit . ,commit)
              (hash . ,source-hash))
            (when (< 0 (length deps))
              `((deps . ,(sort deps 'string<)))))))

(cl-defmethod make-recipe-nix ((info fetch-info))
  "Make a final recipe data used in nix from INFO."
  (with-slots (ename fetcher url repo stable-archive stable-hash
                     unstable-archive unstable-hash)
      info
    (append `((ename . ,ename)
              (fetcher . ,fetcher)
              ,(if (member fetcher '("github"
                                     "bitbucket"
                                     "gitlab"
                                     "sourcehut"
                                     "codeberg"))
                   `(repo . ,repo)
                 `(url . ,url)))
            (unless (equal unstable-hash 'missing)
              `((unstable . ,(source-info unstable-archive
                                          unstable-hash))))
            (unless (equal stable-hash 'missing)
              `((stable . ,(source-info stable-archive
                                        (pcase stable-hash
                                          ('unstable unstable-hash)
                                          (_ stable-hash)))))))))

(cl-defmethod start-fetch ((archives melpa-archive))
  "Start fetch all ARCHIVES and MELPA-ARCHIVE information such as hash."
  (let (recipes-info recipes-nix)
    (setf recipes-info
          (maphash
           (lambda (esym eprops)
             (let* ((ename   (symbol-name esym))
                    (fetcher (gethash 'fetcher eprops))
                    (url     (gethash 'url eprops))
                    (repo    (gethash 'repo eprops))

                    (unstable-aentry (gethash esym
                                       (melpa-archive-unstable archives)))
                    (unstable-aprops  (and unstable-aentry
                                           (gethash 'props unstable-aentry)))
                    (unstable-commit  (and unstable-aprops
                                           (gethash 'commit unstable-aprops)))

                    (stable-aentry (gethash esym
                                            (melpa-archive-stable archives)))
                    (stable-aprops (and stable-aentry
                                        (gethash 'props stable-aentry)))
                    (stable-commit  (and stable-aprops
                                         (gethash 'commit stable-aprops)))

                    (previous (when-let ((previous (melpa-archive-previous
                                                    archives)))
                                (gethash esym previous)))
                    info)
               (setf info (make-fetch-info
                           :ename ename
                           :fetcher fetcher
                           :url url
                           :repo repo
                           :stable-archive stable-aentry
                           :unstable-archive unstable-aentry))

               (when unstable-aprops
                 (let ((*prefetch-info* (make-prefetch-info
                                         :info info
                                         :variant 'unstable)))
                   (source-hash-async (when previous
                                        (gethash 'unstable previous)))))

               (if (equal unstable-commit stable-commit)
                   (setf (fetch-info-stable-hash info) 'unstable)
                 (let ((*prefetch-info* (make-prefetch-info
                                         :info info
                                         :variant 'stable)))
                   (source-hash-async (when previous
                                        (gethash 'stable previous)))))
               info))
           (melpa-archive-recipes archives)))

    (while-let ((info (pop recipes-info)))
      (with-slots (stable-hash unstable-hash) info
        (if (and stable-hash unstable-hash)
            (push (make-recipe-nix info) recipes-nix)
          (progn
            (sleep-for 1)
            (push info (cdr (last recipes-info)))))))

    recipes-nix))

;; ## Emitter

(cl-defmethod emit-json ((archives melpa-archive))
  "Generate the recipes for nix in json with ARCHIVES from melpa."
  (when-let (descriptors (start-fetch archives))
    (message "Finished downloading %d descriptors" (length descriptors))
    (let ((buf (generate-new-buffer "*recipes-archive*")))
      (with-current-buffer buf
        (insert (json-encode descriptors))
        buf))))

(defun get-melpa-json-async (url key archives)
  "Get melpa json data at URL and save it into ARCHIVES at slot named KEY."
  (url-retrieve
   url
   (lambda (status)
     (if-let (err (plist-get status :error))
         (message "Error: failed to get %s due to %s." url err)
       (progn
         (url-http-parse-headers)
         (goto-char url-http-end-of-headers)
         (message (buffer-substring (point-min) (point)))
         (let ((json-object-type 'hash-table)
               (json-array-type 'list)
               (json-key-type 'symbol))
           (setf (cl-struct-slot-value 'melpa-archive
                                       key
                                       archives)
                 (json-read))))))))

;; ## Main runner

(defun run-updater ()
  "Run melpa updater to generate a recipe file in json read by nix."
  (message "Turning off logging to *Message* buffer")
  (let ((message-log-max nil)
        (process-adaptive-read-buffering nil)
        (archives (make-melpa-archive))
        (output-file "recipes-archive-melpa.json")
        recipes-nix-buf)
    (dolist (url '((recipes . "https://melpa.org/recipes.json")
                   (unstable . "https://melpa.org/archive.json")
                   (stable . "https://stable.melpa.org/archive.json")))
      (get-melpa-json-async (cdr url) (car url) archives))
    (setf (melpa-archive-previous archives)
          (parse-previous-archive output-file))

    (while (or (melpa-archive-recipes archives)
               (melpa-archive-stable archives)
               (melpa-archive-unstable archives))
      (sleep-for 1))

    (setf recipes-nix-buf (emit-json archives))

    (with-current-buffer recipes-nix-buf
      (write-file output-file))))

(provide 'update-melpa)

;;; update-melpa.el ends here
