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
        "rbs" = "sudo nixos-rebuild switch --flake $HOME/.config/nixcfg/.#${host}";
        "hms" = "home-manager switch --flake $HOME/nixcfg/.#${config.home.username}@${host}";
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
