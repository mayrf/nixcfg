{ ... }:
{
  flake.modules.homeManager.zsh =
    { config, host, pkgs, ... }:
    {
      features.impermanence.files = [ ".zsh_history" ];
      features.impermanence.directories = [ ".local/share/direnv" ".config/zsh" ];

      home.sessionVariables = {
        FONTS = "$HOME/.local/share/fonts";
        NH_FLAKE = "${host.flakeDir}";
        FLAKE = "${host.flakeDir}";
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

              ec_func_term() {
                  emacsclient -c --no-window-system "$1"
              }

              ec_vanilla_func() {
                  nohup emacsclient -s vanilla -c "$1" >/dev/null 2>&1 &
              }

              send_ghostty_terminfo() {
                  infocmp -x xterm-ghostty | ssh $1 -- tic -x -
              }

              jwt_decode_func() {
                  jq -R 'split(".") |.[0:2] | map(gsub("-"; "+") | gsub("_"; "/") | gsub("%3D"; "=") | @base64d) | map(fromjson)' <<< $1
              }
        '';
      };
      home.shellAliases = {
        vimtutor = "nvim -u NORC -c 'Tutor'";
        rbs = "sudo nixos-rebuild switch --flake $FLAKE#${host.hostName}";
        generate-envrc-flake =
          ''if [ ! -f .envrc ]; then echo "use flake" > .envrc; fi'';
        generate-python-flake =
          "nix flake init --template github:pyproject-nix/pyproject.nix#requirements-txt";
        ls = "eza";
        ps = "procs";
        rbs-no-c =
          "sudo nixos-rebuild switch --flake $FLAKE#${host.hostName} --option build-use-substitutes false";
        nfu = "nix flake update --flake $FLAKE --commit-lock-file";
        optimize = ''
          nix-env --list-generations
          nix-env --delete-generations +1
          sudo nix-collect-garbage -d
          nix-collect-garbage -d
          sudo nix-store --optimise
        '';
        jwt-decode = "jwt_decode_func";
        ghostty-terminfo = "send_ghostty_terminfo";
        fix-nixstore = ''
          sudo nix-store --gc
          nix-store --gc
          sudo nix-store --verify --check-contents --repair
          nix-store --verify --check-contents --repair
        '';
        rlwb = "pkill -USR2 waybar";
        fcd = ''cd "$(find -type d | fzf)"'';
        open = ''xdg-open "$(find -type f | fzf)"'';
        ec = "ec_func";
        ec_term = "ec_func_term";
        ec_van = "ec_vanilla_func";
        k = "kubectl";
        impermanence-check = ''
          sudo fd --one-file-system --base-directory / --type f --hidden --exclude "{tmp,etc/passwd}"'';
      };
    };
}
