{ pkgs ? import <nixpkgs> { } }:

mkShell {
  buildInputs = [
    (python3.withPackages (ps: with ps; [ jupyter ipython black isort ]))
    nodejs
    nodePackages_latest.pyright
  ];
}
