{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.features.ensure-config-repo;
  path = config.hostSpec.flakeDir;
  user = config.hostSpec.username;
in {
  options.features.ensure-config-repo.enable =
    mkEnableOption "my ensure that config repo is present on machine";
  config = mkIf cfg.enable {
    systemd.tmpfiles.rules =
      [ "d  /home/${user}/.config/ 0755 ${user} users -" ];
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
  };
}
