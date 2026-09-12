{ ... }:
{
  flake.modules.homeManager.ensureSecretsRepo =
    { pkgs, config, host, ... }:
    let
      path = "${config.xdg.configHome}/nix-secrets";
      source = "git@codeberg.org:mayrf/nix-secrets.git";
      user = host.username;
    in
    {
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
              ${pkgs.git}/bin/git clone ${source} ${path}
            fi
          ''}";
        };
      };
    };
}
