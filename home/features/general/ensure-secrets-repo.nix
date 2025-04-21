{ pkgs, config, lib, hostSpec, ... }:
with lib;
let
  cfg = config.features.ensure-secrets-repo;
  path = "${config.xdg.configHome}/nix-secrets";
  # path = "/home/${user}/.config/nix-secrets";
  source = "git@codeberg.org:mayrf/nix-secrets.git";
  user = hostSpec.username;
in {
  options.features.ensure-secrets-repo.enable =
    mkEnableOption "my ensure that config secrets repo is present on machine";
  config = mkIf cfg.enable {
    features.impermanence.directories = [ ".config/nix-secrets" ];

    systemd.user.tmpfiles.rules =
      [ "d  /home/${user}/.config/ 0755 ${user} users -" ];
    systemd.user.services.ensure-secrets-repo = {
      Unit = {
        Description = "Ensure nixos config secrets git repository exists.";
      };
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
