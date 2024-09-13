{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {

  packages = [ ];

  # buildInputs = []; Difference between
  #
  # inputsFrom = []; makes all dependencies of the listed packages available in the dev-shell

  shellHook = ''
    # Place shellHook
    # here
    Welcome the your nix-dev-shell!
  '';

  DUMMY_ENVVAR = "NIX";
}
