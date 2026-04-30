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
        inputs.self.modules.homeManager.hmImpermanence
        inputs.self.modules.homeManager.ensureSecretsRepo
        inputs.self.modules.homeManager.ensurePrivateConfigRepo
        inputs.self.modules.homeManager.ensureConfigRepo
        inputs.self.modules.homeManager.zsh
        inputs.self.modules.homeManager.fzf
        inputs.self.modules.homeManager.development
        inputs.self.modules.homeManager.k8s
        inputs.self.modules.homeManager.scripts
        inputs.self.modules.homeManager.yazi
        inputs.self.modules.homeManager.lf
        inputs.self.modules.homeManager.git
        inputs.self.modules.homeManager.hmSops
        inputs.self.modules.homeManager.syncthing
        inputs.self.modules.homeManager.nvim
        inputs.self.modules.homeManager.emacs
        inputs.self.modules.homeManager.vscode
        inputs.self.modules.homeManager.fonts
        inputs.self.modules.homeManager.wayland
        inputs.self.modules.homeManager.waybar
        inputs.self.modules.homeManager.hyprland
        inputs.self.modules.homeManager.gammastep
        inputs.self.modules.homeManager.mako
        inputs.self.modules.homeManager.wofi
        inputs.self.modules.homeManager.nextcloudClient
        inputs.self.modules.homeManager.opencloudClient
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
        inputs.self.modules.homeManager.ghostty
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
