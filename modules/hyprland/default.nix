{ config, pkgs, system, user, hyprland, ... }:

{

  programs.hyprland.package =
    hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
  programs.hyprland = { enable = true; };

  nix.settings = {
    substituters = [ "https://hyprland.cachix.org" ];
    trusted-public-keys =
      [ "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" ];
  };

}
