{ pkgs, hostSpec, ... }:
let host = hostSpec.hostName;
in {
  home.shellAliases = {
    "rbs" = "sudo nixos-rebuild switch --flake $FLAKE#${host}";

    "rbs-no-c" =
      "sudo nixos-rebuild switch --flake $FLAKE#${host} --option build-use-substitutes false";

    "nfu" = "nix flake update --flake $FLAKE --commit-lock-file";

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
    "ec_term" = "ec_func_term";
    "ec_van" = "ec_vanilla_func";
    "k" = "kubectl";
    "check-impermanence" = ''
      sudo fd --one-file-system --base-directory / --type f --hidden --exclude "{tmp,etc/passwd}"'';
  };
  home.sessionVariables = {
    FONTS = "$HOME/.local/share/fonts";
    FLAKE = if host != "yttrium" then
      "$HOME/.config/nixcfg"
    else
      "${hostSpec.flakeDir}";
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
  };
}
