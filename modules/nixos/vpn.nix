{ config, pkgs, lib, ... }:
with lib;
let cfg = config.mymodules.vpn;
in {
  options.mymodules.vpn = {
    enable = mkEnableOption "my vpn config";
    configFile = mkOption {
      type = types.path;
      description = ''
        A Wireguard.conf file
      '';
    };
  };
  config = mkIf cfg.enable {

    systemd.services."wg-quick-wg0" = {
      requires = [ "network-online.target" ];
      after = [ "network.target" "network-online.target" ];
      wantedBy = lib.mkForce [ ];
      environment.DEVICE = "wg0";
      path = [ pkgs.wireguard-tools pkgs.iptables pkgs.iproute ];
    };

    networking = {
      wg-quick.interfaces = {
        wg0 = {
          configFile = cfg.configFile;
          # <<kill-switch>>
          postUp = ''
            iptables -I OUTPUT ! -o wg0 -m mark ! --mark $(wg show wg0 fwmark) -m addrtype ! --dst-type LOCAL -j REJECT  && ip6tables -I OUTPUT ! -o wg0 -m mark ! --mark $(wg show wg0 fwmark) -m addrtype ! --dst-type LOCAL -j REJECT
          '';

          preDown = ''
            iptables -D OUTPUT ! -o wg0 -m mark ! --mark $(wg show wg0 fwmark) -m addrtype ! --dst-type LOCAL -j REJECT && ip6tables -D OUTPUT ! -o wg0 -m mark ! --mark $(wg show wg0 fwmark) -m addrtype ! --dst-type LOCAL -j REJECT
          '';

        };
      };
    };
  };
}
