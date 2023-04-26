import ./. rec {
  shell = true;
  pkgs = import (builtins.fetchTarball {
      # latest https://github.com/NixOS/nixpkgs/commits/nixos-22.11 as of Wed 26 Apr 2023 06:32:17 PM EDT
      url = "https://github.com/NixOS/nixpkgs/archive/60c0f762658916a4a5b5a36b3e06486f8301daf4.tar.gz";
      sha256 = "0z6jy05yawj2pkinpyjlpym5niij2scxn6cd2w1p9wrfxw0hw8ra";
    } {});
}
