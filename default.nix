{ config ? { }
, pkgs ? import (builtins.fetchTarball {
    # latest https://github.com/NixOS/nixpkgs/commits/nixos-22.11 as of Wed 26 Apr 2023 06:32:17 PM EDT
    url = "https://github.com/NixOS/nixpkgs/archive/60c0f762658916a4a5b5a36b3e06486f8301daf4.tar.gz";
    sha256 = "0z6jy05yawj2pkinpyjlpym5niij2scxn6cd2w1p9wrfxw0hw8ra";
  }) { inherit config; }
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
    git-config = with pkgs.haskell.lib; doJailbreak (dontCheck (markUnbroken haskellPackages.git-config));
  };
  env = drv.envFunc { withHoogle = true; };
in

if shell
then withNativeInputs [ pkgs.ghcid ] env
# nix-shell --run "runhaskell Setup.hs configure && runhaskell Setup.hs repl"
# nix-shell --run "runhaskell Setup.hs configure && ghcid -c 'runhaskell Setup.hs repl'"

else drv
