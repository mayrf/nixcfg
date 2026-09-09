{ inputs, ... }:
{
  imports = [
    inputs.flake-parts.flakeModules.modules
    inputs.wrapper-modules.flakeModules.wrappers
    # ./hosts.nix   # or keep your existing hosts.nix path
  ];

  systems = [ "x86_64-linux" ];
}
