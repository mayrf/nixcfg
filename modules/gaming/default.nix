{ ... }:
{
  flake.modules.nixos.gaming =
    { pkgs, ... }:
    {
      hardware.graphics = {
        enable = true;
        enable32Bit = true;
      };
      services.xserver.videoDrivers = [ "amdgpu" ];
      programs.steam.enable = true;
      programs.steam.protontricks.enable = true;
      programs.steam.gamescopeSession.enable = true;
      programs.steam.extraCompatPackages = with pkgs; [ proton-ge-bin ];
      environment.systemPackages = with pkgs; [
        mangohud
        protonup-ng
        # lutris
        heroic
        wine
      ];
      systemd.settings.Manager = {
        DefaultLimitNOFILE = "1048576";
      };
      systemd.user.extraConfig = ''
        DefaultLimitNOFILE=1048576
      '';
      programs.gamemode.enable = true;
      environment.sessionVariables = {
        STEAM_EXTRA_COMPAT_TOOLS_PATHS = "\${HOME}/.steam/root/compatibilitytools.d";
      };
    };
}
