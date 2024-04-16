let
  pkgs = import ../../../../.. {};

  emacsEnv = pkgs.emacs;

in pkgs.mkShell {
  packages = [
    pkgs.nix
    pkgs.bash
    pkgs.nix-prefetch-git
    pkgs.nix-prefetch-hg
    emacsEnv
  ];
}
