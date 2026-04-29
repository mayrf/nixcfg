{ inputs, self, ... }:
{
  flake.modules.nixos.common =
    { config, pkgs, outputs, lib, ... }:
    let
      username = config.hostSpec.username;
      ifTheyExist = groups:
        builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
      pubKeys = lib.filesystem.listFilesRecursive ../common/keys;
      useSops = config.sops.secrets ? "${username}/hashedPassword";
    in
    {
      imports = [
        inputs.home-manager.nixosModules.home-manager
        ../host-spec.nix
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
      networking.hostName = config.hostSpec.hostName;
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
      // lib.optionalAttrs (config.hostSpec.isMinimal) {
        password = "nixos";
      }
      // lib.optionalAttrs useSops {
        hashedPasswordFile =
          config.sops.secrets."${username}/hashedPassword".path;
      }
      // lib.optionalAttrs (!config.hostSpec.isMinimal && !useSops) {
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
          hostSpec = config.hostSpec;
        };
      };
      home-manager.users.root =
        lib.optionalAttrs (!config.hostSpec.isMinimal) {
          home.stateVersion = config.hostSpec.sysStateVersion;
        };
    };
}
