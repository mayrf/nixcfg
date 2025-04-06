{ config, lib, pkgs, ... }:

{
  stylix.enable = true;
  stylix.image = pkgs.fetchurl {
    url =
      "https://unsplash.com/photos/K2s_YE031CA/download?ixid=M3wxMjA3fDB8MXxhbGx8fHx8fHx8fHwxNzE5NDI1ODU3fA&force=true&w=2400";
    sha256 = "sha256-mVRwIHcjWbyPSzN5L7/F50DWs2f+MZJ6dSh3pyjmPys=";
  };
  # stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/woodland.yaml";
  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml";
  # https://nixos.wiki/wiki/GNOME#Running_GNOME_programs_outside_of_GNOME
  environment.systemPackages =
    [ pkgs.adwaita-icon-theme pkgs.gnomeExtensions.appindicator ];
  services.udev.packages = [ pkgs.gnome-settings-daemon ];
}
