{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.laptop;
in {
  options.features.laptop = { enable = mkEnableOption "my laptop config"; };
  config = mkIf cfg.enable {

    # Better scheduling for CPU cycles - thanks System76!!!
    services.system76-scheduler.settings.cfsProfiles.enable = true;

    # Enable TLP (better than gnomes internal power manager)
    #
    # 10.04.25: Breaks because of dependency devscripts_2.23.7.tar.xz being unavailable
    services.tlp = {
      enable = lib.mkForce false;
      # settings = {
      #   CPU_BOOST_ON_AC = 1;
      #   CPU_BOOST_ON_BAT = 0;
      #   CPU_SCALING_GOVERNOR_ON_AC = "performance";
      #   CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
      # };
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

    # Disable GNOMEs power management
    services.power-profiles-daemon.enable = false;

    # Enable powertop
    powerManagement.powertop.enable = true;

    # Enable thermald (only necessary if on Intel CPUs)
    services.thermald.enable = true;

  };
}
