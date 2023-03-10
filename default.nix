{ config ? { allowBroken = true; }
, pkgs ? import <nixpkgs> { inherit config; }
, shell ? false
}:

let
  ghc = "ghc810";
  src = pkgs.nix-gitignore.gitignoreSource [ "*.cabal" "dist/" "*.nix" "result" ] ./.;
  drv = pkgs.haskell.packages.${ghc}.callCabal2nix "mog" src { };
  env = drv.envFunc { withHoogle = true; };
in

if shell
then env
else drv
