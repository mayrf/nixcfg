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
    inherit (inputs.dotfiles-private) private;
  };
in
{
  flake.modules.nixos.helium =
    { config, pkgs, ... }:
    {
      imports = [
        self.modules.nixos.base
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
        ../../../hosts/helium
      ];
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
