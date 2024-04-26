lib: self:

let

    fetcherGenerators = { repo ? null
                        , url ? null
                        , ... }:
                        { hash
                        , commit
                        , ...}: {
      github = self.callPackage ({ fetchFromGitHub }:
        fetchFromGitHub {
          owner = lib.head (lib.splitString "/" repo);
          repo = lib.head (lib.tail (lib.splitString "/" repo));
          rev = commit;
          inherit hash;
        }
      ) {};
      gitlab = self.callPackage ({ fetchFromGitLab }:
        fetchFromGitLab {
          owner = lib.head (lib.splitString "/" repo);
          repo = lib.head (lib.tail (lib.splitString "/" repo));
          rev = commit;
          inherit hash;
        }
      ) {};
      git = self.callPackage ({ fetchgit }:
        (fetchgit {
          rev = commit;
          inherit hash url;
        }).overrideAttrs(_: {
          GIT_SSL_NO_VERIFY = true;
        })
      ) {};
      bitbucket = self.callPackage ({ fetchhg }:
        fetchhg {
          rev = commit;
          url = "https://bitbucket.com/${repo}";
          inherit hash;
        }
      ) {};
      hg = self.callPackage ({ fetchhg }:
        fetchhg {
          rev = commit;
          inherit hash url;
        }
      ) {};
      sourcehut = self.callPackage ({ fetchzip }:
        fetchzip {
          url = "https://git.sr.ht/~${repo}/archive/${commit}.tar.gz";
          inherit hash;
        }
      ) {};
      codeberg = self.callPackage ({ fetchzip }:
        fetchzip {
          url = "https://codeberg.org/${repo}/archive/${commit}.tar.gz";
          inherit hash;
        }
      ) {};
    };

in {
  elpaDerivation = { ename, version, url, hash, deps, homepage, description, ...
                   }:
  let
    broken = hash == null;
    reqs = if deps != null then deps else [];
    pname = ename;
  in
    lib.nameValuePair ename (
      self.callPackage ({ fetchelpa, elpaBuild, ... }@pkgargs:
        elpaBuild {
          inherit ename pname version;

          src = if broken then null else fetchelpa {
            inherit url hash;
          };

          packageRequires = map (dep: pkgargs.${dep} or self.${dep} or null) reqs;

          meta = {
            inherit homepage broken description;
            license = lib.licenses.free;
          };
        }
      ) {}
    );

  melpaDerivation = variant:
    { ename, fetcher, recipe ? null, ... }@args:
      let
        sourceArgs = args.${variant};
        version = sourceArgs.version or "";
        reqs = if sourceArgs.deps != null then sourceArgs.deps else [ ];
        hasSource = lib.hasAttr variant args;
        pname = builtins.replaceStrings [ "@" ] [ "at" ] ename;
        broken = sourceArgs.hash == null;
        inherit (sourceArgs) commit;
      in
      if hasSource then
        lib.nameValuePair ename (
          self.callPackage ({ melpaBuild, writeText, ... }@pkgargs:
            melpaBuild {
              inherit pname ename commit version;
              # TODO: Broken should not result in src being null (hack to avoid
              # eval errors).
              src = if broken then null else
                lib.getAttr fetcher (fetcherGenerators args sourceArgs);
              recipe = if recipe == null then null else
                writeText "${pname}-recipe" recipe;
              packageRequires =
                map (dep: pkgargs.${dep} or self.${dep} or null) reqs;
              meta = (sourceArgs.meta or {}) // {
                inherit broken;
              };
            }
          ) {}
      )
    else
      null;

}
