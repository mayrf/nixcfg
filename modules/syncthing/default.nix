{ ... }:
{
  flake.modules.homeManager.syncthing =
    { config, ... }:
    {
      features.impermanence.directories = [ ".local/state/syncthing/" ];
      services.syncthing = {
        enable = true;
        guiAddress = "127.0.0.1:8384";
        overrideDevices = false;
        overrideFolders = false;
        settings = {
          options.urAccepted = -1;
          gui = {
            enabled = true;
            tls = true;
          };
        };
      };
      systemd.user.services.syncthing.Service.Environment = [
        "all_proxy=socks5://localhost:1080"
      ];
    };
}
