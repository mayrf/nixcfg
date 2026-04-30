{ inputs, self, ... }:
{
  flake.modules.homeManager.commonHome =
    { inputs, lib, pkgs, config, outputs, host, ... }:
    let inherit (inputs.nix-colors) colorSchemes;
    in
    {
      imports = [
        inputs.nix-colors.homeManagerModule
      ];

      services = {
        gpg-agent = {
          enable = true;
          pinentry.package = pkgs.pinentry-qt;
        };
      };

      programs.ssh.includes = [ "~/.ssh/config.local" ];

      home = {
        username = host.username;
        homeDirectory = lib.mkDefault "/home/${host.username}";
        sessionPath = [ "$HOME/.local/bin" ];
      };

      features.impermanence.directories = [
        ".ssh"
        ".gnupg"
        ".local/share/keyrings"
        "Downloads"
        "Documents"
        "playground"
        "code"
        "cloud"
        ".local/share/fonts"
      ];

      home.file = {
        ".local/bin" = {
          source = ../common/scripts;
          recursive = true;
        };
      };

      nix = {
        settings = {
          warn-dirty = false;
          keep-outputs = true;
          experimental-features = [ "nix-command" "flakes" ];
        };

        registry = {
          "mytemplates" = {
            from = {
              id = "mytemplates";
              type = "indirect";
            };
            to = {
              path = "${config.xdg.configHome}/nixcfg";
              type = "path";
            };
          };
        };
      };
      systemd.user.startServices = "sd-switch";

      programs = { home-manager.enable = true; };

      home.file.".colorscheme".text = config.colorscheme.slug;

      fonts.fontconfig.enable = true;

      xdg = {
        enable = true;
        configHome = "${config.home.homeDirectory}/.config";
        cacheHome = "${config.home.homeDirectory}/.cache";
        dataHome = "${config.home.homeDirectory}/.local/share";
        stateHome = "${config.home.homeDirectory}/.local/state";
      };

      xdg.mimeApps.defaultApplications = {
        "application/pdf" = "org.pwmt.zathura.desktop";
        "video/x-msvideo" = "vlc.desktop";
        "x-scheme-handler/postman" = "Postman.desktop";
        "x-scheme-handler/msteams" = "teams.desktop";
        "x-scheme-handler/http" = "librewolf.desktop";
        "x-scheme-handler/https" = "librewolf.desktop";
        "x-scheme-handler/mailto" = "userapp-Thunderbird-08TXD2.desktop";
        "message/rfc822" = "userapp-Thunderbird-08TXD2.desktop";
        "x-scheme-handler/mid" = "userapp-Thunderbird-08TXD2.desktop";
        "application/vnd.oasis.opendocument.text" = "writer.desktop";
        "application/vnd.oasis.opendocument.spreadsheet" = "calc.desktop";
        "text/csv" = "calc.desktop";
        "application/vnd.oasis.opendocument.base" = "base.desktop";
        "application/msword" = "writer.desktop";
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document" =
          "writer.desktop";
        "image/jpeg" = "sxiv.desktop";
        "image/png" = "sxiv.desktop";
        "image/gif" = "sxiv.desktop";
        "text/plain" = "emacsclient.desktop";
        "text/markdown" = "emacsclient.desktop";
        "text/x-markdown" = "emacsclient.desktop";
        "text/yaml" = "emacsclient.desktop";
        "text/x-yaml" = "emacsclient.desktop";
        "application/x-yaml" = "emacsclient.desktop";
        "text/x-config" = "emacsclient.desktop";
        "text/x-ini" = "emacsclient.desktop";
        "text/x-python" = "emacsclient.desktop";
        "text/x-shellscript" = "emacsclient.desktop";
        "text/x-script" = "emacsclient.desktop";
        "application/x-shellscript" = "emacsclient.desktop";
        "application/json" = "emacsclient.desktop";
        "text/json" = "emacsclient.desktop";
        "text/xml" = "emacsclient.desktop";
        "application/xml" = "emacsclient.desktop";
      };
    };

  flake.modules.nixos.common =
    { config, pkgs, outputs, lib, ... }:
    let
      username = config.host.username;
      ifTheyExist = groups:
        builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
      pubKeys = lib.filesystem.listFilesRecursive ../common/keys;
      useSops = config.sops.secrets ? "${username}/hashedPassword";
    in
    {
      imports = [
        inputs.home-manager.nixosModules.home-manager
        inputs.sops-nix.nixosModules.sops
        inputs.nixos-wsl.nixosModules.wsl
        inputs.stylix.nixosModules.stylix
        inputs.disko.nixosModules.default
        inputs.impermanence.nixosModules.impermanence
        inputs.nixos-cli.nixosModules.nixos-cli
        self.modules.nixos.hostSpec
      ];

      # nixos-cli
      programs.nixos-cli = {
        enable = true;
        settings.option.min_score = 2;
      };

      # Nix settings
      nix = {
        nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
        settings = {
          experimental-features = [ "nix-command" "flakes" ];
          keep-outputs = true;
          trusted-users = [ "root" "${username}" ];
          substituters = [
            "https://watersucks.cachix.org"
            "https://hyprland.cachix.org"
            "https://nix-community.cachix.org/"
            "https://nixpkgs-ruby.cachix.org"
          ];
          trusted-public-keys = [
            "watersucks.cachix.org-1:6gadPC5R8iLWQ3EUtfu3GFrVY7X6I4Fwz/ihW25Jbv8="
            "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "nixpkgs-ruby.cachix.org-1:vrcdi50fTolOxWCZZkw0jakOnUI1T19oYJ+PRYdK4SM="
          ];
        };
      };
      networking.networkmanager.enable = true;
      time.timeZone = "Europe/Berlin";
      i18n.defaultLocale = "en_US.UTF-8";
      i18n.extraLocales = [ "de_DE.UTF-8/UTF-8" ];

      # Hardware
      hardware.graphics.enable = true;

      # Console / keyboard
      console.useXkbConfig = true;

      # Common programs
      programs.git.enable = true;
      programs.zsh.enable = true;
      programs.dconf.enable = true;
      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
      };

      # Services
      services.openssh.enable = true;
      services.openssh.settings.PasswordAuthentication = false;
      services.locate.enable = true;
      services.gnome.gnome-keyring.enable = true;
      services.gvfs.enable = true;

      # Security
      security.polkit.enable = true;
      security.pam.services.hyprlock = { };
      security.sudo.wheelNeedsPassword = false;
      security.rtkit.enable = true;

      # Kernel
      boot.kernelPackages = pkgs.unstable.linuxPackages_zen;

      # Environment
      environment.variables = {
        TERMINAL = "ghostty";
        BROWSER = "librewolf";
        EDITOR = "nvim";
        VISUAL = "nvim";
      };
      environment.pathsToLink = [ "/share/zsh" ];

      # System packages
      environment.systemPackages = with pkgs; [
        file
        urlencode
        nmap
        mlocate
        openssl
        tldr
        unzip
        htop
        trash-cli
        jq
        yq
        tree
        zip
        rsync
        fd
        busybox
        vim
        wget
        pass
        gnupg
        stable.nushell
        borgbackup
        borgmatic
        sshfs
      ];

      # Fonts
      fonts.packages = with pkgs; [
        vista-fonts
        noto-fonts
        noto-fonts-cjk-sans
        noto-fonts-color-emoji
        liberation_ttf
        fira-code
        fira-code-symbols
        mplus-outline-fonts.githubRelease
        dina-font
        proggyfonts
        roboto
      ];

      # Nixpkgs
      nixpkgs = {
        overlays = [
          inputs.nur.overlays.default
          outputs.overlays.additions
          outputs.overlays.stable-packages
          outputs.overlays.unstable-packages
          inputs.emacs-overlay.overlay
        ];
        config = {
          allowUnfree = true;
          allowUnfreePredicate = (_: true);
        };
      };

      # User
      users.mutableUsers = !useSops;
      users.users.${username} = {
        home = "/home/${username}";
        isNormalUser = true;
        description = "${username}";
        shell = pkgs.zsh;
        packages = [ pkgs.home-manager ];
        openssh.authorizedKeys.keys =
          lib.lists.forEach pubKeys (key: builtins.readFile key);
        extraGroups = lib.flatten [
          "wheel"
          (ifTheyExist [
            "flatpak" "docker" "input" "kvm" "qemu-libvirtd" "plugdev"
            "audio" "video" "git" "networkmanager" "network"
            "scanner" "lp" "libvirtd" "deluge"
          ])
        ];
      }
      // lib.optionalAttrs (config.host.isMinimal) {
        password = "nixos";
      }
      // lib.optionalAttrs useSops {
        hashedPasswordFile =
          config.sops.secrets."${username}/hashedPassword".path;
      }
      // lib.optionalAttrs (!config.host.isMinimal && !useSops) {
        initialPassword = "changeme";
      };

      systemd.tmpfiles.rules =
        let
          user = config.users.users.${username}.name;
          group = config.users.users.${username}.group;
        in
        [
          "d /home/${username}/.ssh 0750 ${user} ${group} -"
          "d /home/${username}/.ssh/sockets 0750 ${user} ${group} -"
        ];

      # Root user
      users.users.root = {
        hashedPasswordFile =
          config.users.users.${username}.hashedPasswordFile;
        password = lib.mkForce config.users.users.${username}.password;
        openssh.authorizedKeys.keys =
          config.users.users.${username}.openssh.authorizedKeys.keys;
      };

      # Home Manager wiring (common config for all users)
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        extraSpecialArgs = {
          inherit inputs outputs;
          inherit (inputs.dotfiles-private) private;
          host = config.host // { hostName = config.networking.hostName; };
        };
      };
      home-manager.users.root =
        lib.optionalAttrs (!config.host.isMinimal) {
          home.stateVersion = config.system.stateVersion;
        };
      home-manager.users.${username} = {
        home.stateVersion = config.system.stateVersion;
        imports = [ self.modules.homeManager.commonHome ];
      };
    };
}
