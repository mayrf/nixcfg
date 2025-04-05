{ pkgs, config, lib, ... }:
let
  path = config.hostSpec.flakeDir;
  user = config.hostSpec.username;
in {
  systemd.tmpfiles.rules = [
    "d  /home/${user}/.config/ 0755 ${user} users -"
    # "d  /home/${user}/.config/nixcfg 0755 mayrf users -"
  ];
  systemd.services.ensure-git-repo = {
    description = "Ensure nixos condig git repository exists";
    after = [ "network-online.target" ]; # Ensure network is up
    wants = [ "network-online.target" ];
    unitConfig = { StartLimitIntervalSec = 0; };
    serviceConfig = {
      Type = "oneshot";
      User = "${user}"; # Or a specific user
      Group = "users";
    };
    script = ''
      if [ -z "$(ls -A ${path})" ]; then
        # Directory is empty
        "${pkgs.git}/bin/git clone https://github.com/mayrf/nixcfg ${path}"
      fi
    '';
    wantedBy = [ "multi-user.target" ];
  };
}
