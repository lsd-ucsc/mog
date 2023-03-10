{ config ? { }
, pkgs ? import <nixpkgs> { inherit config; }
, shell ? false
}:

let
  ghc = "ghc810";
  haskellPackages = pkgs.haskell.packages.${ghc};
  withNativeInputs = pkgs: d: d.overrideAttrs (old: { nativeBuildInputs = old.nativeBuildInputs ++ pkgs; });
  src = pkgs.nix-gitignore.gitignoreSource [ "*.cabal" "dist/" "*.nix" "result" ] ./.;
  drv = haskellPackages.callCabal2nix "mog" src {
    gitlib-libgit2 = pkgs.haskell.lib.markUnbroken haskellPackages.gitlib-libgit2;
  };
  env = drv.envFunc { withHoogle = true; };
in

if shell
then withNativeInputs [ pkgs.ghcid ] env # runhaskell Setup.hs configure && ghcid -c 'runhaskell Setup.hs repl'
else drv
