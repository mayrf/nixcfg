{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  packages = with pkgs; [
    python311
    poetry
    nodePackages_latest.pyright
    python311Packages.isort
    python311Packages.pylint
    python311Packages.yapf
    python311Packages.pylama
  ];
}
