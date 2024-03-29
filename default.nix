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
  # deps
  gitlib-src = pkgs.fetchFromGitHub {
    owner = "jwiegley";
    repo = "gitlib";
    rev = "9d6d2aee8ed6012ea50bbcde80c47ecf83a5a595";
    sha256 = "055n0g4s6bcrhagm9np1jhvpavcpni3anpfqw9x8c4rxwk0f2ywi";
  };
  gitlib = haskellPackages.callCabal2nix "gitlib" "${gitlib-src}/gitlib" {};
  gitlib-test = haskellPackages.callCabal2nix "gitlib-test" "${gitlib-src}/gitlib-test" { inherit gitlib; };
  gitlib-libgit2 = haskellPackages.callCabal2nix "gitlib-libgit2" "${gitlib-src}/gitlib-libgit2" { inherit gitlib; inherit gitlib-test; };
  # package
  src = pkgs.nix-gitignore.gitignoreSource [ "*.cabal" "dist/" "*.nix" "result" ] ./.;
  drv = haskellPackages.callCabal2nix "mog" src {
    inherit gitlib;
    inherit gitlib-libgit2;
    git-config = with pkgs.haskell.lib; doJailbreak (dontCheck (markUnbroken haskellPackages.git-config));
  };
  env = drv.envFunc { withHoogle = true; };
in

if shell
then withNativeInputs [ pkgs.ghcid ] env
# nix-shell --run "runhaskell Setup.hs configure && runhaskell Setup.hs repl"
# nix-shell --run "runhaskell Setup.hs configure && ghcid -c 'runhaskell Setup.hs repl'"

else drv
