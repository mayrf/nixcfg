{ config, lib, pkgs, ... }:

{
  programs = {
    foot = {
      enable = true;
      server.enable = true;
      settings = {
        main = {
          term = "xterm-256color";

          font = "JetBrainsMono Nerd Font:size=10";
          dpi-aware = "yes";
        };
        mouse = {
          hide-when-typing = "yes";
        };
      };
    };
  };
}
