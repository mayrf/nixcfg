# You can build these directly using 'nix build .#example'

{ pkgs ? import <nixpkgs> { } }: rec {

  kcl-language-server = pkgs.callPackage ./kcl-language-server { };

}
