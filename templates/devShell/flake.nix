
# See flake.lock for the specific Git revisions that our flake inputs are
# pinned to.
{
  description = "Generic nix dev shell";

  # Flake inputs (the Nix code sources we need to rely on here)
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  # Flake outputs (the stuff we want to provide in this flake)
  outputs = { self, nixpkgs, flake-utils }:
    # Per-system outputs for the default systems here:
    # https://github.com/numtide/flake-utils/blob/master/default.nix#L3-L9
    #
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Custom Nixpkgs for the current system
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
      in {
        # Cross-platform development environment (including CI)
        devShells.default = pkgs.mkShell {
          # Packages available in the environment
          buildInputs = with pkgs; [

          ];
          shellHook = ''
            Welcome the your nix-dev-shell!
          '';
        };
      });
}
}
