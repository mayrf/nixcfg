{ lib, ... }:
{
  flake.modules.nixos.laptop =
    { pkgs, ... }:
    {
      services.system76-scheduler.settings.cfsProfiles.enable = true;
      services.tlp = {
        enable = lib.mkForce true;
      };
      services.auto-cpufreq.enable = true;
      services.auto-cpufreq.settings = {
        battery = {
          governor = "powersave";
          turbo = "never";
          energy_performance_preference = "power";
        };
        charger = {
          governor = "powersave";
          turbo = "never";
          energy_performance_preference = "power";
        };
      };
      services.power-profiles-daemon.enable = false;
      powerManagement.powertop.enable = true;
      services.thermald.enable = true;
      environment.systemPackages = [ pkgs.lm_sensors ];
    };
}
