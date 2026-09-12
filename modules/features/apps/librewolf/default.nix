{ ... }:
{
  flake.modules.homeManager.librewolf =
    { config, pkgs, ... }:
    {
      features.impermanence.directories = [
        ".librewolf"
        ".cache/librewolf"
      ];
      programs.librewolf.enable = true;
      programs.librewolf.package = pkgs.stable.librewolf;
    };
}
