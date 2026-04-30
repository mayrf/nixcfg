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
        ../../_home/general/impermanence.nix
        ../../_home/general/ensure-secrets-repo.nix
        ../../_home/general/ensure-private-config-repo.nix
        ../../_home/general/ensure-config-repo.nix
        ../../_home/cli/zsh.nix
        ../../_home/cli/fzf.nix
        ../../_home/cli/development.nix
        ../../_home/cli/k8s.nix
        ../../_home/cli/scripts
        ../../_home/cli/yazi.nix
        ../../_home/cli/lf
        ../../_home/cli/git
        ../../_home/cli/sops.nix
        ../../_home/cli/syncthing.nix
        ../../_home/editor/nvim.nix
        ../../_home/editor/emacs
        ../../_home/editor/vscode.nix
        ../../_home/desktop/fonts.nix
        ../../_home/desktop/wayland.nix
        ../../_home/desktop/waybar.nix
        ../../_home/desktop/hyprland.nix
        ../../_home/desktop/gammastep.nix
        ../../_home/desktop/mako.nix
        ../../_home/desktop/wofi.nix
        ../../_home/desktop/nextcloud-client.nix
        ../../_home/desktop/opencloud-client.nix
        ../../_home/desktop/virtualisation.nix
        ../../_home/desktop/postman.nix
        ../../_home/desktop/librewolf.nix
        ../../_home/desktop/gpg.nix
        ../../_home/desktop/zathura.nix
        ../../_home/desktop/learning.nix
        ../../_home/desktop/media.nix
        ../../_home/desktop/social.nix
        ../../_home/desktop/productivity.nix
        ../../_home/desktop/zen-browser.nix
        ../../_home/terminal/alacritty.nix
        ../../_home/terminal/ghostty.nix
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
