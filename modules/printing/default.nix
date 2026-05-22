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

      hardware.sane = {
        enable = true;
        brscan4 = {
          enable = true;
          netDevices = {
            office1 = {
              ip = "192.168.0.109";
              model = "MFC-L2800DW";
            };
          };
        };
      };
    };
}
