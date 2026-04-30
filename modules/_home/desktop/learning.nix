{ config, pkgs, ... }:
{
  features.impermanence.directories = [ ".local/share/Anki2" ];
  home.packages = with pkgs; [ anki-bin tipp10 ];
}
