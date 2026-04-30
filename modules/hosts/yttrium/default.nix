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
        ../../../home/features/general/impermanence.nix
        ../../../home/features/general/ensure-secrets-repo.nix
        ../../../home/features/general/ensure-private-config-repo.nix
        ../../../home/features/general/ensure-config-repo.nix
        ../../../home/features/cli/zsh.nix
        ../../../home/features/cli/fzf.nix
        ../../../home/features/cli/ai.nix
        ../../../home/features/cli/media.nix
        ../../../home/features/cli/development.nix
        ../../../home/features/cli/k8s.nix
        ../../../home/features/cli/leetcode.nix
        ../../../home/features/cli/yazi.nix
        ../../../home/features/cli/scripts
        ../../../home/features/cli/lf
        ../../../home/features/cli/git
        ../../../home/features/cli/syncthing.nix
        ../../../home/features/cli/sops.nix
        ../../../home/features/editor/emacs
        ../../../home/features/editor/nvim.nix
        ../../../home/features/editor/vscode.nix
        ../../../home/features/editor/zed.nix
        ../../../home/features/desktop/fonts.nix
        ../../../home/features/desktop/wayland.nix
        ../../../home/features/desktop/waybar.nix
        ../../../home/features/desktop/hyprland.nix
        ../../../home/features/desktop/gammastep.nix
        ../../../home/features/desktop/mako.nix
        ../../../home/features/desktop/wofi.nix
        ../../../home/features/desktop/nextcloud-client.nix
        ../../../home/features/desktop/virtualisation.nix
        ../../../home/features/desktop/postman.nix
        ../../../home/features/desktop/librewolf.nix
        ../../../home/features/desktop/gpg.nix
        ../../../home/features/desktop/zathura.nix
        ../../../home/features/desktop/learning.nix
        ../../../home/features/desktop/media.nix
        ../../../home/features/desktop/social.nix
        ../../../home/features/desktop/productivity.nix
        ../../../home/features/desktop/zen-browser.nix
        ../../../home/features/terminal/alacritty.nix
        ../../../home/features/terminal/foot.nix
        ../../../home/features/terminal/ghostty.nix
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
        self.modules.nixos.commonModules
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
