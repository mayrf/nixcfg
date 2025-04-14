{ pkgs, config, lib, hostSpec, ... }:
with lib;
let
  cfg = config.features.ensure-config-repo;
  path = "${config.xdg.configHome}/nixcfg";
  # path = "/home/${user}/.config/nix-secrets";
  source = "git@github.com:mayrf/nixcfg.git";
  user = hostSpec.username;
in {
  options.features.ensure-config-repo.enable =
    mkEnableOption "my ensure that config secrets repo is present on machine";
  config = mkIf cfg.enable {
    systemd.user.tmpfiles.rules =
      [ "d  /home/${user}/.config/ 0755 ${user} users -" ];
    systemd.user.services.ensure-config-repo = {
      Unit = { Description = "Ensure nixos config git repository exists."; };
      Install = { WantedBy = [ "default.target" ]; };
      Service = {
        ExecStart = "${pkgs.writeShellScript "watch-store" ''
          #!/run/current-system/sw/bin/bash
          if [ ! -d "${path}" ] || [ -z "$(ls -A ${path})" ]; then
            # Directory is empty
            ${pkgs.git}/bin/git clone ${source} ${path}
          fi
        ''}";
      };
    };
  };
}
