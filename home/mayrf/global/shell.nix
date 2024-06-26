{ config, lib, pkgs, host, ... }:

{
  home.shellAliases = {
    "rbs" = "sudo nixos-rebuild switch --flake $HOME/.config/nixcfg/.#${host}";
    "nfu" = "nix flake update /home/mayrf/.config/nixcfg --commit-lock-file";

    "optimize" = ''
      nix-env --list-generations
            nix-env --delete-generations +1
            sudo nix-collect-garbage -d
            nix-collect-garbage -d
            sudo nix-store --optimise'';
    # "emacs" = "emacsclient -c";
    "rlwb" = "pkill -USR2 waybar";
    "fcd" = ''cd "$(find -type d | fzf)"'';
    "open" = ''xdg-open "$(find -type f | fzf)"'';
    "ec" = "ec_func";
    "k" = "kubectl";
  };
  home.sessionVariables = {
    FONTS = "$HOME/.local/share/fonts";
    FLAKE = "/home/mayrf/.config/nixcfg";
  };
  home.packages = with pkgs; [ nh nix-output-monitor nvd ];
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
      autocd = true;
      historySubstringSearch.enable = true;
      history = {
        ignoreDups = true;
        size = 100000;
      };
      initExtra = ''
        source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
        eval "$(direnv hook zsh)"

        # functions
        ec_func() {
            nohup emacsclient -c "$1" >/dev/null 2>&1 &
        }

      '';
    };
  };
}
