{ inputs, self, ... }:
let
  specialArgs = {
    outputs = inputs.self.outputs;
    inherit inputs;
    inherit (inputs.dotfiles-private) private;
  };
in
{
  flake.modules.homeManager.yttrium =
    { config, inputs, pkgs, ... }:
    {
      imports = [
        inputs.self.modules.homeManager.hmImpermanence
        inputs.self.modules.homeManager.ensureSecretsRepo
        inputs.self.modules.homeManager.ensurePrivateConfigRepo
        inputs.self.modules.homeManager.ensureConfigRepo
        inputs.self.modules.homeManager.zsh
        inputs.self.modules.homeManager.fzf
        inputs.self.modules.homeManager.ai
        inputs.self.modules.homeManager.cliMedia
        inputs.self.modules.homeManager.development
        inputs.self.modules.homeManager.k8s
        inputs.self.modules.homeManager.leetcode
        inputs.self.modules.homeManager.yazi
        inputs.self.modules.homeManager.scripts
        inputs.self.modules.homeManager.lf
        inputs.self.modules.homeManager.git
        inputs.self.modules.homeManager.syncthing
        inputs.self.modules.homeManager.hmSops
        inputs.self.modules.homeManager.emacs
        inputs.self.modules.homeManager.nvim
        inputs.self.modules.homeManager.vscode
        inputs.self.modules.homeManager.zed
        inputs.self.modules.homeManager.fonts
        inputs.self.modules.homeManager.wayland
        inputs.self.modules.homeManager.waybar
        inputs.self.modules.homeManager.hyprland
        inputs.self.modules.homeManager.gammastep
        inputs.self.modules.homeManager.mako
        inputs.self.modules.homeManager.wofi
        inputs.self.modules.homeManager.nextcloudClient
        inputs.self.modules.homeManager.virtualisation
        inputs.self.modules.homeManager.postman
        inputs.self.modules.homeManager.librewolf
        inputs.self.modules.homeManager.gpg
        inputs.self.modules.homeManager.zathura
        inputs.self.modules.homeManager.learning
        inputs.self.modules.homeManager.desktopMedia
        inputs.self.modules.homeManager.social
        inputs.self.modules.homeManager.productivity
        inputs.self.modules.homeManager.zenBrowser
        inputs.self.modules.homeManager.alacritty
        inputs.self.modules.homeManager.foot
        inputs.self.modules.homeManager.ghostty
        inputs.dotfiles-private.outputs.homeManagerModules
        "${inputs.dotfiles-private}/home/desktop-apps.nix"
      ];

      colorscheme = inputs.nix-colors.colorschemes.woodland;

      features.impermanence.enable = true;
      features.impermanence.directories_cache = [
        ".local/share/docker"
      ];
      features.impermanence.directories = [
        ".cursor"
        ".config/Cursor"
      ];

      wayland.windowManager.hyprland.settings = {
        monitor = [
          "DP-5,2560x1440@60,0x0,1"
          "DP-4,2560x1440@60,0x0,1"
          "DP-2,2560x1440@60,2560x0,1"
          "HDMI-A-1,1920x1080@120,auto-left,1"
          ",preferred,auto-right,1"
        ];
        workspace = [
          "1, monitor:DP-4, default:true"
          "2, monitor:DP-4"
          "3, monitor:DP-4"
          "4, monitor:DP-4"
          "5, monitor:DP-4"
          "1, monitor:DP-5, default:true"
          "2, monitor:DP-5"
          "3, monitor:DP-5"
          "4, monitor:DP-5"
          "5, monitor:DP-5"
          "6, monitor:DP-2"
          "7, monitor:DP-2"
          "8, monitor:DP-2"
          "9, monitor:DP-2"
          "10, monitor:DP-2"
          "F1, monitor:HDMI-A-1"
          "F2, monitor:HDMI-A-1"
          "F3, monitor:HDMI-A-1"
        ];
      };

      home.packages = with pkgs; [
        urbit
        exercism
        vimgolf
        img2pdf
        gparted
        code-cursor
        rustdesk-flutter
        anydesk
        stable.teams-for-linux
      ];

      features.private.ssh.enable = true;
      features.private.personal.enable = true;
    };

  flake.modules.nixos.yttrium =
    { config, pkgs, ... }:
    {
      imports = [
        self.modules.nixos.base
        self.modules.nixos.common
        self.modules.nixos.impermanence
        self.modules.nixos.emacs
        self.modules.nixos.claude
        self.modules.nixos.sops
        self.modules.nixos.bluetooth
        self.modules.nixos.docker
        self.modules.nixos.flatpak
        self.modules.nixos.gaming
        self.modules.nixos.kanata
        self.modules.nixos.open-webui
        self.modules.nixos.pipewire
        self.modules.nixos.printing
        self.modules.nixos.theming
        self.modules.nixos.virtualisation
        self.modules.nixos.winapps
        ./_hardware-configuration.nix
        ./_immich-ml-server.nix
        inputs.dotfiles-private.outputs.nixosModules
        (import ./_disko.nix { device = "/dev/nvme0n1"; })
      ];

      host = {
        username = "mayrf";
        persistDir = "/persist";
        isImpermanent = true;
      };
      networking.hostName = "yttrium";
      system.stateVersion = "26.05"; # Did you read the comment?

      features.private = {
        common.enable = true;
        vpn.enable = true;
        mount-data.enable = true;
      };

      persistence.enable = true;
      persistence.user = config.preferences.user.name;
      persistence.directories = [
        "/var/lib/private/open-webui"
        { directory = "/var/lib/private"; mode = "u=rwx,g=,o="; }
        { directory = "/var/lib/private/ollama"; mode = "0700"; }
      ];

      services.ollama = {
        rocmOverrideGfx = "10.3.0";
        package = pkgs.ollama-rocm;
        enable = true;
        environmentVariables = {
          OLLAMA_CONTEXT_LENGTH = "8192";
        };
      };

      environment.systemPackages = [
        pkgs.nfs-utils
        pkgs.pavucontrol
      ];

      hardware.sane = {
        enable = true;
        brscan4 = {
          enable = true;
          netDevices = {
            office1 = {
              ip = "192.168.0.109";
              model = "MFC-L2800DW";
            };
          };
        };
      };

      services.avahi.enable = true;
      services.avahi.nssmdns4 = true;
      security.sudo.enable = true;

      home-manager.users.${config.host.username}.imports = [
        self.modules.homeManager.yttrium
      ];
    };

  flake.nixosConfigurations.yttrium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules = [ self.modules.nixos.yttrium ];
  };
}
