# You can build these directly using 'nix build .#example'

{ pkgs ? import <nixpkgs> { }, ... }: rec {
  auger = pkgs.callPackage ./auger { };
  just-lsp = pkgs.callPackage ./just-lsp { };
  argocd-lovely-plugin = pkgs.callPackage ./argocd-lovely-plugin { };
}
