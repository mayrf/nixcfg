{ ... }:
{
  flake.modules.nixos.theming =
    { pkgs, ... }:
    {
      environment.systemPackages = [
        pkgs.adwaita-icon-theme
        pkgs.gnomeExtensions.appindicator
      ];
      services.udev.packages = [ pkgs.gnome-settings-daemon ];
    };
}
