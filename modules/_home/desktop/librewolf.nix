{ config, pkgs, ... }:
{
  features.impermanence.directories =
    [ ".librewolf" ".mozilla" ".cache/librewolf" ];
  programs.librewolf.enable = true;
  programs.librewolf.package = pkgs.stable.librewolf;
  stylix.targets.librewolf.enable = false;
}
