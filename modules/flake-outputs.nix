{ inputs, ... }:
{
  imports = [
    inputs.flake-parts.flakeModules.modules
    # ./hosts.nix   # or keep your existing hosts.nix path
  ];

  systems = [ "x86_64-linux" ];
}
