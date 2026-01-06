{
  description = "A very basic flake";
  inputs.utils.url = "github:numtide/flake-utils";

  # inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  outputs = { self, nixpkgs, utils, }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          # config.allowUnfree = true;
        };
      in {
        devShells.default = pkgs.mkShell rec {

          buildInputs = with pkgs; [ ];

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;

        };
      });
}
