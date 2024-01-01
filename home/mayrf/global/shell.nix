{ config, lib, pkgs, host, ... }:

{
  programs = {

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    starship = {
      enableZshIntegration = true;
      enable = true;
      settings = { add_newline = false; };
    };

    zsh = {
      enable = true;
      shellAliases = {
        "rbs" =
          "sudo nixos-rebuild switch --flake $HOME/.config/nixcfg/.#${host}";
        "nfu" = "nix flake update /home/mayrf/.config/nixcfg";
        "emacs" = "emacsclient -c";
        "rlwb" = "pkill -USR2 waybar";
      };
      autocd = true;
      historySubstringSearch.enable = true;
      history = {
        ignoreDups = true;
        size = 100000;
      };
      initExtra = ''
        eval "$(direnv hook zsh)"
      '';
    };
  };
}
