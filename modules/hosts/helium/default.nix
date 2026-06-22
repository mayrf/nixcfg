{
  inputs,
  self,
  lib,
  ...
}:
let
  specialArgs = {
    outputs = inputs.self.outputs;
    inherit inputs;
  };
in
{
  flake.modules.homeManager.helium =
    { inputs, ... }:
    {
      imports = [
        self.modules.homeManager.ensureSecretsRepo
        self.modules.homeManager.ensurePrivateConfigRepo
        self.modules.homeManager.ensureConfigRepo
        self.modules.homeManager.zsh
        self.modules.homeManager.fzf
        self.modules.homeManager.development
        self.modules.homeManager.k8s
        self.modules.homeManager.scripts
        self.modules.homeManager.yazi
        self.modules.homeManager.lf
        self.modules.homeManager.git
        self.modules.homeManager.hmSops
        self.modules.homeManager.syncthing
        self.modules.homeManager.nvim
        self.modules.homeManager.emacs
        self.modules.homeManager.vscode
        self.modules.homeManager.fonts
        self.modules.homeManager.hyprland
        self.modules.homeManager.gammastep
        self.modules.homeManager.wofi
        self.modules.homeManager.nextcloudClient
        self.modules.homeManager.opencloudClient
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
        self.modules.homeManager.ghostty
        inputs.dotfiles-private.modules.homeManager.helium
      ];

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

    };

  flake.modules.nixos.helium =
    { config, pkgs, ... }:
    {
      imports = [
        self.modules.nixos.base
        self.modules.nixos.general
        self.modules.nixos.common
        self.modules.nixos.impermanence
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
        inputs.dotfiles-private.modules.nixos.helium
        inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x220
        (import ./_disko.nix { device = "/dev/sda"; })
      ];

      home-manager.users.${config.host.username}.imports = [
        self.modules.homeManager.helium
        self.modules.homeManager.hmImpermanence
      ];

      host = {
        username = "mayrf";
        persistDir = "/persist";
        isImpermanent = true;
      };

      networking.hostName = "helium";
      system.stateVersion = "26.11"; # Did you read the comment?

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
        after = [
          "network.target"
          "network-online.target"
        ];
        wantedBy = lib.mkForce [ ];
        environment.DEVICE = "wg0";
        path = [
          pkgs.wireguard-tools
          pkgs.iptables
          pkgs.iproute2
        ];
      };

    };

  flake.nixosConfigurations.helium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules = [ self.modules.nixos.helium ];
  };
}
