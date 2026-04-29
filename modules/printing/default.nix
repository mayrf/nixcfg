{ ... }:
{
  flake.modules.nixos.printing =
    { pkgs, ... }:
    {
      persistence.directories = [
        "/var/cache/cups"
        "/var/lib/cups"
      ];
      services.printing.enable = true;
      services.printing.drivers = [
        pkgs.gutenprint
        pkgs.brlaser
        pkgs.brgenml1lpr
        pkgs.brgenml1cupswrapper
      ];
      environment.systemPackages = [ pkgs.system-config-printer ];
    };
}
