{ pkgs, config, hostSpec, ... }:
let
  path = "${config.xdg.configHome}/nixcfg";
  source = "git@github.com:mayrf/nixcfg.git";
  user = hostSpec.username;
in {
  features.impermanence.directories = [ ".config/nixcfg" ];
  systemd.user.tmpfiles.rules =
    [ "d  /home/${user}/.config/ 0755 ${user} users -" ];
  systemd.user.services.ensure-config-repo = {
    Unit = { Description = "Ensure nixos config git repository exists."; };
    Install = { WantedBy = [ "default.target" ]; };
    Service = {
      ExecStart = "${pkgs.writeShellScript "watch-store" ''
        #!/run/current-system/sw/bin/bash
        if [ ! -d "${path}" ] || [ -z "$(ls -A ${path})" ]; then
          ${pkgs.git}/bin/git clone ${source} ${path}
        fi
      ''}";
    };
  };
}
