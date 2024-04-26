/*

# Updating

To update the list of packages from nongnu (ELPA),

1. Run `./update-nongnu`.
2. Check for evaluation errors:
     # "../../../../../" points to the default.nix from root of Nixpkgs tree
     env NIXPKGS_ALLOW_BROKEN=1 nix-instantiate ../../../../../ -A emacs.pkgs.nongnuPackages
3. Run `git commit -m "nongnu-packages $(date -Idate)" -- nongnu-generated.nix`

*/

{ lib, buildPackages, stdenv, texinfo, writeText, gcc }:

self: let

  # Use custom elpa url fetcher with fallback/uncompress
  fetchElpa = self.buildPackages.callPackage ./fetchelpa.nix { };

  elpaBuild = import ../../../../build-support/emacs/elpa.nix {
    inherit lib stdenv texinfo writeText gcc;
    inherit (self) emacs;
  };

  generateNongnu = lib.makeOverridable ({
    archiveJson ? ./recipes-archive-nongnu.json
  }: let

    inherit (import ./libgenerated.nix lib self) elpaDerivation;

    super = lib.listToAttrs (builtins.filter
      (s: s != null)
      (map elpaDerivation (lib.importJSON archiveJson))
    );

    overrides = {
    };

    nongnuPackages = super // overrides;
  in nongnuPackages // { inherit elpaBuild fetchElpa; });

in generateNongnu { }
