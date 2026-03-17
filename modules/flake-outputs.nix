{ inputs, ... }:
{
  imports = [
    # ./hosts.nix   # or keep your existing hosts.nix path
  ];

  flake = {
    templates = import ../templates;
    overlays  = import ../overlays { inherit inputs; };
  };

  systems = [ "x86_64-linux" ];
}
