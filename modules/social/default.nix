{ ... }:
{
  flake.modules.homeManager.social =
    { config, pkgs, ... }:
    {
      features.impermanence.directories = [ ".config/Signal" ];
      home.packages = with pkgs; [ signal-desktop hexchat ];
    };
}
