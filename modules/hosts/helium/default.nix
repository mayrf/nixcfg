{ inputs, self, lib, ... }:
let
  specialArgs = {
    outputs = inputs.self.outputs;
    inherit inputs;
    inherit (inputs.dotfiles-private) private;
  };
in
{
  flake.modules.homeManager.helium =
    { inputs, ... }:
    {
      imports = [
        ../../../home/features/general/impermanence.nix
        ../../../home/features/general/ensure-secrets-repo.nix
        ../../../home/features/general/ensure-private-config-repo.nix
        ../../../home/features/general/ensure-config-repo.nix
        ../../../home/features/cli/zsh.nix
        ../../../home/features/cli/fzf.nix
        ../../../home/features/cli/development.nix
        ../../../home/features/cli/k8s.nix
        ../../../home/features/cli/scripts
        ../../../home/features/cli/yazi.nix
        ../../../home/features/cli/lf
        ../../../home/features/cli/git
        ../../../home/features/cli/sops.nix
        ../../../home/features/cli/syncthing.nix
        ../../../home/features/editor/nvim.nix
        ../../../home/features/editor/emacs
        ../../../home/features/editor/vscode.nix
        ../../../home/features/desktop/fonts.nix
        ../../../home/features/desktop/wayland.nix
        ../../../home/features/desktop/waybar.nix
        ../../../home/features/desktop/hyprland.nix
        ../../../home/features/desktop/gammastep.nix
        ../../../home/features/desktop/mako.nix
        ../../../home/features/desktop/wofi.nix
        ../../../home/features/desktop/nextcloud-client.nix
        ../../../home/features/desktop/opencloud-client.nix
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
        ../../../home/features/terminal/ghostty.nix
        inputs.dotfiles-private.outputs.homeManagerModules
        "${inputs.dotfiles-private}/home/desktop-apps.nix"
      ];

      colorscheme = inputs.nix-colors.colorschemes.woodland;

      features.impermanence.enable = true;

      wayland.windowManager.hyprland.settings = {
        monitor = [ "LVDS-1,1366x768@60,0x0,1" ];
        workspace = [
          "1, monitor:LVDS-1, default:true"
          "2, monitor:LVDS-1"
          "3, monitor:LVDS-1"
          "4, monitor:LVDS-1"
          "5, monitor:LVDS-1"
          "6, monitor:LVDS-1"
          "7, monitor:LVDS-1"
        ];
      };

      features.private.ssh.enable = true;
    };

  flake.modules.nixos.helium =
    { config, pkgs, ... }:
    {
      imports = [
        self.modules.nixos.base
        self.modules.nixos.common
        self.modules.nixos.impermanence
        self.modules.nixos.commonModules
        self.modules.nixos.sops
        self.modules.nixos.bluetooth
        self.modules.nixos.docker
        self.modules.nixos.flatpak
        self.modules.nixos.kanata
        self.modules.nixos.laptop
        self.modules.nixos.pipewire
        self.modules.nixos.printing
        self.modules.nixos.theming
        ./_hardware-configuration.nix
        ./_distributed-builds.nix
        inputs.dotfiles-private.outputs.nixosModules
        inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x220
        (import ./_disko.nix { device = "/dev/sda"; })
      ];

      host = {
        username = "mayrf";
        persistDir = "/persist";
        isImpermanent = true;
      };
      networking.hostName = "helium";
      system.stateVersion = "26.05"; # Did you read the comment?

      features.private = {
        common.enable = true;
        vpn.enable = true;
      };

      persistence.enable = true;
      persistence.user = config.preferences.user.name;

      # VPN (wireguard) — configFile comes from sops secret
      sops.secrets."wireguard/x220_conf" = { };
      networking.wg-quick.interfaces.wg0 = {
        configFile = config.sops.secrets."wireguard/x220_conf".path;
        postUp = ''
          iptables -I OUTPUT ! -o wg0 -m mark ! --mark $(wg show wg0 fwmark) -m addrtype ! --dst-type LOCAL -j REJECT  && ip6tables -I OUTPUT ! -o wg0 -m mark ! --mark $(wg show wg0 fwmark) -m addrtype ! --dst-type LOCAL -j REJECT
        '';
        preDown = ''
          iptables -D OUTPUT ! -o wg0 -m mark ! --mark $(wg show wg0 fwmark) -m addrtype ! --dst-type LOCAL -j REJECT && ip6tables -D OUTPUT ! -o wg0 -m mark ! --mark $(wg show wg0 fwmark) -m addrtype ! --dst-type LOCAL -j REJECT
        '';
      };
      systemd.services."wg-quick-wg0" = {
        requires = [ "network-online.target" ];
        after = [ "network.target" "network-online.target" ];
        wantedBy = lib.mkForce [ ];
        environment.DEVICE = "wg0";
        path = [ pkgs.wireguard-tools pkgs.iptables pkgs.iproute2 ];
      };

      home-manager.users.${config.host.username}.imports = [
        self.modules.homeManager.helium
      ];
    };

  flake.nixosConfigurations.helium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules = [ self.modules.nixos.helium ];
  };
}
