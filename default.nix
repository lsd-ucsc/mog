{ config ? { }
, pkgs ? import <nixpkgs> { inherit config; }
, shell ? false
}:

let
  # setup & util
  ghc = "ghc810";
  haskellPackages = pkgs.haskell.packages.${ghc};
  withNativeInputs = pkgs: d: d.overrideAttrs (old: { nativeBuildInputs = old.nativeBuildInputs ++ pkgs; });
  # package
  src = pkgs.nix-gitignore.gitignoreSource [ "*.cabal" "dist/" "*.nix" "result" ] ./.;
  drv = haskellPackages.callCabal2nix "mog" src {
    gitlib-libgit2 = pkgs.haskell.lib.markUnbroken haskellPackages.gitlib-libgit2;
    cryptonite = haskellPackages.callHackage "cryptonite" "0.30" { };
  };
  env = drv.envFunc { withHoogle = true; };
in

if shell
then withNativeInputs [ pkgs.ghcid ] env
# nix-shell --run "runhaskell Setup.hs configure && runhaskell Setup.hs repl"
# nix-shell --run "runhaskell Setup.hs configure && ghcid -c 'runhaskell Setup.hs repl'"

else drv
