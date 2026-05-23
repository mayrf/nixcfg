{ inputs, self, ... }:
let
  specialArgs = {
    outputs = inputs.self.outputs;
    inherit inputs;
  };
in
{
  flake.modules.homeManager.yttrium =
    {
      config,
      inputs,
      pkgs,
      ...
    }:
    {
      imports = [
        self.modules.homeManager.ensureSecretsRepo
        self.modules.homeManager.ensurePrivateConfigRepo
        self.modules.homeManager.ensureConfigRepo
        self.modules.homeManager.zsh
        self.modules.homeManager.fzf
        self.modules.homeManager.ai
        self.modules.homeManager.cliMedia
        self.modules.homeManager.development
        self.modules.homeManager.k8s
        self.modules.homeManager.leetcode
        self.modules.homeManager.yazi
        self.modules.homeManager.scripts
        self.modules.homeManager.lf
        self.modules.homeManager.git
        self.modules.homeManager.syncthing
        self.modules.homeManager.hmSops
        self.modules.homeManager.emacs
        self.modules.homeManager.nvim
        self.modules.homeManager.vscode
        self.modules.homeManager.zed
        self.modules.homeManager.fonts
        self.modules.homeManager.hyprland
        self.modules.homeManager.gammastep
        self.modules.homeManager.wofi
        self.modules.homeManager.nextcloudClient
        self.modules.homeManager.virtualisation
        self.modules.homeManager.postman
        self.modules.homeManager.librewolf
        self.modules.homeManager.gpg
        self.modules.homeManager.zathura
        self.modules.homeManager.learning
        self.modules.homeManager.desktopMedia
        self.modules.homeManager.social
        self.modules.homeManager.productivity
        self.modules.homeManager.zenBrowser
        self.modules.homeManager.alacritty
        self.modules.homeManager.foot
        self.modules.homeManager.ghostty
        inputs.dotfiles-private.modules.homeManager.yttrium
      ];

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
        zoom-us
      ];

    };

  flake.modules.nixos.yttrium =
    { config, pkgs, ... }:
    {
      imports = [
        self.modules.nixos.base
        self.modules.nixos.common
        self.modules.nixos.general
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
        inputs.dotfiles-private.modules.nixos.yttrium
        (import ./_disko.nix { device = "/dev/nvme0n1"; })
      ];
      home-manager.users.${config.host.username}.imports = [
        self.modules.homeManager.hmImpermanence
        self.modules.homeManager.yttrium
      ];

      host = {
        username = "mayrf";
        persistDir = "/persist";
        isImpermanent = true;
      };
      networking.hostName = "yttrium";
      system.stateVersion = "26.05"; # Did you read the comment?

      persistence.enable = true;
      persistence.user = config.preferences.user.name;
      persistence.directories = [
        "/var/lib/private/open-webui"
        {
          directory = "/var/lib/private";
          mode = "u=rwx,g=,o=";
        }
        {
          directory = "/var/lib/private/ollama";
          mode = "0700";
        }
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

      services.avahi.enable = true;
      services.avahi.nssmdns4 = true;
      security.sudo.enable = true;

    };

  flake.nixosConfigurations.yttrium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules = [ self.modules.nixos.yttrium ];
  };
}
