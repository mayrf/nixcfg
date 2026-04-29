{ ... }:
{
  flake.modules.nixos.theming =
    { pkgs, ... }:
    {
      stylix.enable = true;
      stylix.targets.qt.enable = false;
      stylix.image = pkgs.fetchurl {
        url = "https://unsplash.com/photos/K2s_YE031CA/download?ixid=M3wxMjA3fDB8MXxhbGx8fHx8fHx8fHwxNzE5NDI1ODU3fA&force=true&w=2400";
        sha256 = "sha256-mVRwIHcjWbyPSzN5L7/F50DWs2f+MZJ6dSh3pyjmPys=";
      };
      stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-hard.yaml";
      environment.systemPackages = [
        pkgs.adwaita-icon-theme
        pkgs.gnomeExtensions.appindicator
      ];
      services.udev.packages = [ pkgs.gnome-settings-daemon ];
    };
}
