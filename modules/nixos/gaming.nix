{ config, pkgs, lib, ... }:
with lib;
let cfg = config.mymodules.gaming;
in {
  options.mymodules.gaming = { enable = mkEnableOption "my gaming config"; };
  config = mkIf cfg.enable {
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
    };

    # hardware.opengl has beed changed to hardware.graphics

    services.xserver.videoDrivers = [ "amdgpu" ];

    # services.xserver.videoDrivers = ["nvidia"];
    # hardware.nvidia.modesetting.enable = true;
    #
    programs.steam.enable = true;
    programs.steam.gamescopeSession.enable = true;

    environment.systemPackages = with pkgs; [
      mangohud
      protonup
      lutris
      heroic
      bottles
    ];

    programs.gamemode.enable = true;

    environment.sessionVariables = {
      STEAM_EXTRA_COMPAT_TOOLS_PATHS =
        "\${HOME}/.steam/root/compatibilitytools.d";
    };
    # gamescope -W 2560 -H 1440 -r 60 -- %command%

  };
}
