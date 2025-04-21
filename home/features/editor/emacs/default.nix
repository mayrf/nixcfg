{ config, pkgs, lib, private, hostSpec, ... }:
with lib;
let
  cfg = config.features.editor.emacs;

  # package = pkgs.emacs-unstable;
  # emacs = pkgs.emacs-unstable.override {
  # withXwidgets = true;
  # withGTK3 = true;
  # };
  # emacs = ((pkgs.emacsPackagesFor pkgs.emacs29).emacsWithPackages
  emacs = ((pkgs.emacsPackagesFor pkgs.emacs-git).emacsWithPackages
    # (epkgs: [ epkgs.vterm epkgs.emacsql-sqlite epkgs.pdf-tools ]));
    (epkgs: [ epkgs.vterm epkgs.emacsql epkgs.pdf-tools ]));
  repoUrl = "https://github.com/doomemacs/doomemacs";
  emacsBinPath = "${emacs}/bin";
  # Match the default socket path for the Emacs version so emacsclient continues
  # to work without wrapping it.
  socketDir = "%t/emacs";
in {
  options.features.editor.emacs.enable = mkEnableOption "my emacs user config";
  config = mkIf cfg.enable {
    features.impermanence.directories = [
      # ".config/emacs"
      ".config/emacs-doom"
    ];

    # Emacs
    services.emacs = {
      enable = true;
      package = emacs;
    };

    programs.emacs = {
      enable = true;
      package = emacs;
    };

    # systemd.user.services.vanillaemacs = {
    #   Unit = {
    #     Description = "Emacs text editor with vanilla config";
    #     Documentation =
    #       "info:emacs man:emacs(1) https://gnu.org/software/emacs/";

    #     # After = optional (cfg.startWithUserSession == "graphical")
    #     #   "graphical-session.target";
    #     # PartOf = optional (cfg.startWithUserSession == "graphical")
    #     #   "graphical-session.target";

    #     # Avoid killing the Emacs session, which may be full of
    #     # unsaved buffers.
    #     X-RestartIfChanged = false;
    #     # } // optionalAttrs needsSocketWorkaround {
    #     # Emacs deletes its socket when shutting down, which systemd doesn't
    #     # handle, resulting in a server without a socket.
    #     # See https://github.com/nix-community/home-manager/issues/2018
    #     # RefuseManualStart = true;
    #   };

    #   Service = {
    #     Type = "notify";

    #     # We wrap ExecStart in a login shell so Emacs starts with the user's
    #     # environment, most importantly $PATH and $NIX_PROFILES. It may be
    #     # worth investigating a more targeted approach for user services to
    #     # import the user environment.
    #     ExecStart = ''
    #       ${pkgs.runtimeShell} -l -c "${emacsBinPath}/emacs --init-directory ${config.xdg.configHome}/emacs-vanilla --fg-daemon=vanilla"
    #     '';

    #     # Emacs will exit with status 15 after having received SIGTERM, which
    #     # is the default "KillSignal" value systemd uses to stop services.
    #     SuccessExitStatus = 15;

    #     Restart = "on-failure";
    #     # } // optionalAttrs needsSocketWorkaround {
    #     # Use read-only directory permissions to prevent emacs from
    #     # deleting systemd's socket file before exiting.
    #     # ExecStartPost = "${pkgs.coreutils}/bin/chmod --changes -w ${socketDir}";
    #     # ExecStopPost = "${pkgs.coreutils}/bin/chmod --changes +w ${socketDir}";
    #   };
    #   # } // optionalAttrs (cfg.startWithUserSession != false) {
    #   Install = {
    #     WantedBy = [
    #       # (if cfg.startWithUserSession == true then
    #       "default.target"
    #       # else
    #       # "graphical-session.target")
    #     ];
    #   };
    # };

    home.activation = let
      source = "${hostSpec.flakeDir}/home/features/editor/emacs/vanilla";
      target = "${config.xdg.configHome}/emacs";
    in {
      emacsActivationAction = ''
                link_repo() {
        	  rm -r ${target}
                  ln -sf ${source} ${target}
                }
                if [ ! -d "${target}" ] || [ -z "$(ls "${target}")" ]; then 
                    link_repo
                elif [ ! -L "${target}" ]; then
                    TEMP_DIR=$(mktemp -d)
                    mv ${target}/{.,}* $TEMP_DIR
                    link_repo
                    mv $TEMP_DIR/{.,}* ${target}
                    rm -r $TEMP_DIR
                fi
      '';
      doomEmacsActivationAction = ''
        check_dir() {
            local dir="$1"
            [ ! -d "$dir" ] || [ -z "$(ls "$dir")" ]
        }

        # EMACS_DIR="${config.xdg.configHome}/emacs"
        DOOM_EMACS_DIR="${config.xdg.configHome}/emacs-doom"
        DOOM="${config.xdg.configHome}/doom"

        if check_dir "$DOOM_EMACS_DIR"; then
            rm -rf $DOOM_EMACS_DIR/*
            ${pkgs.git}/bin/git clone --depth=1 --single-branch "${repoUrl}" $DOOM_EMACS_DIR
        fi


        # if check_dir "$EMACS_DIR"; then
        #   ln -s ${hostSpec.flakeDir}/home/features/editor/emacs/vanilla $EMACS_DIR
        # fi

        if [ ! -e "$DOOM" ]; then
          ln -s ${hostSpec.flakeDir}/home/features/editor/emacs/doom $DOOM
          # yes | $EMACS_DIR/bin/doom install
        fi
      '';
    };
    home.sessionVariables = {
      EMACS_DIR = "${config.xdg.configHome}/emacs";
      DOOM = "${config.xdg.configHome}/doom";
      DOOMDIR = "${config.xdg.configHome}/doom";
    };
    # fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    home.packages = with pkgs; [
      # Doom emacs dependencies
      lldb
      git
      (ripgrep.override { withPCRE2 = true; })
      gnutls # for TLS connectivity
      coreutils
      fd # faster projectile indexing
      imagemagick # for image-dired    sqlite
      gcc
      zstd # for undo-fu-session/undo-tree compression

      # shell mode
      shfmt
      shellcheck
      bash-language-server

      # web mode
      html-tidy

      # docker mode
      dockfmt

      # :tools editorconfig
      editorconfig-core-c # per-project style config

      #misc/unknown
      stylelint
      vimPlugins.copilot-vim

      unstable.nerd-fonts.im-writing
      unstable.nerd-fonts.jetbrains-mono
      unstable.nerd-fonts.symbols-only

      # :markdown preview
      python311Packages.grip

      # for markdown-preview-eww
      rubyPackages.redcarpet
      yaml-language-server

      # nix
      nixd

      # typescript
      nodePackages_latest.eslint
      nodePackages_latest.prettier
      prettier-plugin-go-template
      nodePackages_latest.typescript-language-server

      # language tools
      languagetool

      # other dependencies
      hunspell
      hunspellDicts.de_AT
      hunspellDicts.de_DE
      hunspellDicts.hu_HU
      hunspellDicts.en_US
      hunspellDicts.es_ES
      hunspellDicts.en_GB-ize

      # python
      stable.poetry
      ruff
      python311Packages.isort
      python311Packages.pylint
      python311Packages.yapf
      python311Packages.pylama
      vscode-langservers-extracted

      xclip
      poppler_utils

      # go
      gopls

      # just
      just
      # just-lsp

      #fonts
      unstable.nerd-fonts.im-writing
      unstable.nerd-fonts.jetbrains-mono
      unstable.nerd-fonts.fira-code
      unstable.nerd-fonts.caskaydia-cove
      unstable.nerd-fonts.geist-mono

      # ai 
      # aider-chat
    ];

    sops.secrets."emacs/authinfo" = { };

    # if check_dir "$EMACS_DIR"; then
    #   ln -s ${hostSpec.flakeDir}/modules/home-manager/emacs/vanilla $EMACS_DIR
    # fi

    xdg.configFile."emacs/.env".text = ''
      WORK_GITFORGE_HOST=${private.work.gitForgeHost}
      EMACS_AUTHINFO_PATH=${config.sops.secrets."emacs/authinfo".path}
    '';
    programs.git.extraConfig = {
      gitlab.${private.work.gitForgeHost}.user = "${private.work.gitUser}";
    };

    home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    home.shellAliases = {
      "emacs" = "${emacs}/bin/emacs";
      "doom-emacs" =
        "${emacs}/bin/emacs --init-directory ${config.xdg.configHome}/emacs-doom &";
    };
    # e()     { pgrep emacs && emacsclient -n "$@" || emacs -nw "$@" }
    # ediff() { emacs -nw --eval "(ediff-files \"$1\" \"$2\")"; }
    # eman()  { emacs -nw --eval "(switch-to-buffer (man \"$1\"))"; }
    # ekill() { emacsclient --eval '(kill-emacs)'; }
  };
}
