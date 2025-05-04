{ config, lib, hostSpec, pkgs, ... }:
with lib;
let cfg = config.features.cli.zsh;
in {
  options.features.cli.zsh.enable =
    mkEnableOption "enable extended zsh configuration";

  config = mkIf cfg.enable {

    features.impermanence.files = [ ".zsh_history" ];

    features.impermanence.directories = [ ".local/share/direnv" ];

    home.sessionVariables = {
      FONTS = "$HOME/.local/share/fonts";
      NH_FLAKE = "${hostSpec.flakeDir}";
    };
    home.packages = with pkgs; [ nh nix-output-monitor nvd ];
    programs.starship = {
      enableZshIntegration = true;
      enable = true;
      settings = { add_newline = false; };
    };

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    programs.zsh = {
      enable = true;
      autocd = true;
      historySubstringSearch.enable = true;
      autosuggestion.enable = true;
      history = {
        ignoreDups = true;
        size = 100000;
      };
      initContent = ''
            source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
            eval "$(direnv hook zsh)"

        #     set -x NIX_PATH nixpkgs=channel:nixos-unstable
        #     set -x NIX_LOG info
        #     set -x TERMINAL kitty
        #     if test (tty) = "/dev/tty1"
        #       exec Hyprland &> /dev/null
        #     end

            # functions
            function y() {
              local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
              yazi "$@" --cwd-file="$tmp"
              if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
                builtin cd -- "$cwd"
              fi
              rm -f -- "$tmp"
            }

            ec_func() {
                nohup emacsclient -c "$1" >/dev/null 2>&1 &
            }

            # functions
            ec_func_term() {
                emacsclient -c --no-window-system "$1"
            }

            # functions
            ec_vanilla_func() {
                nohup emacsclient -s vanilla -c "$1" >/dev/null 2>&1 &
            }
      '';
    };
    home.shellAliases = {
      "rbs" = "sudo nixos-rebuild switch --flake $FLAKE#${hostSpec.hostName}";

      ls = "eza";
      grep = "rg";
      ps = "procs";
      "rbs-no-c" =
        "sudo nixos-rebuild switch --flake $FLAKE#${hostSpec.hostName} --option build-use-substitutes false";

      "nfu" = "nix flake update --flake $FLAKE --commit-lock-file";

      "optimize" = ''
        nix-env --list-generations
        nix-env --delete-generations +1
        sudo nix-collect-garbage -d
        nix-collect-garbage -d
        sudo nix-store --optimise
      '';
      "fix-nixstore" = ''
        sudo nix-store --verify --check-contents
        sudo nix-store --gc
      '';
      # "emacs" = "emacsclient -c";
      "rlwb" = "pkill -USR2 waybar";
      "fcd" = ''cd "$(find -type d | fzf)"'';
      "open" = ''xdg-open "$(find -type f | fzf)"'';
      "ec" = "ec_func";
      "ec_term" = "ec_func_term";
      "ec_van" = "ec_vanilla_func";
      "k" = "kubectl";
      "impermanence-check" = ''
        sudo fd --one-file-system --base-directory / --type f --hidden --exclude "{tmp,etc/passwd}"'';
    };
  };
}
