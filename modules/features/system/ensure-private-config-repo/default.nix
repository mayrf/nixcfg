{ ... }:
{
  flake.modules.homeManager.ensurePrivateConfigRepo =
    { pkgs, config, host, ... }:
    let
      path = "${config.xdg.configHome}/dotfiles-private";
      source = "git@codeberg.org:mayrf/dotfiles-private.git";
      user = host.username;
    in
    {
      features.impermanence.directories = [ ".config/dotfiles-private" ];
      systemd.user.tmpfiles.rules =
        [ "d  /home/${user}/.config/ 0755 ${user} users -" ];
      systemd.user.services.ensure-private-config-repo = {
        Unit = {
          Description = "Ensure nixos private config git repository exists.";
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
