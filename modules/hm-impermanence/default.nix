{ ... }:
{
  flake.modules.homeManager.hmImpermanence =
    { config, pkgs, lib, host, inputs, ... }:
    with lib;
    let cfg = config.features.impermanence;
    in
    {
      options.features.impermanence = {
        enable = mkEnableOption "my impermanence config";
        directories = mkOption {
          default = [ ];
          example = [
            "Downloads"
            "Music"
            "Pictures"
            "Documents"
            "Videos"
            ".gnupg"
            ".ssh"
            ".local/share/keyrings"
            ".local/share/direnv"
            {
              directory = ".local/share/Steam";
              method = "symlink";
            }
          ];
          description = ''
            A list of directories in your home directory that
            you want to link to persistent storage. You may optionally
            specify the linking method each directory should use.
          '';
        };

        directories_cache = mkOption {
          type = with types; listOf str;
          default = [ ];
          example = [
            "Downloads"
            "Music"
            "Pictures"
            "Documents"
            "Videos"
            ".gnupg"
            ".ssh"
            ".local/share/keyrings"
            ".local/share/direnv"
            {
              directory = ".local/share/Steam";
              method = "symlink";
            }
          ];
          description = ''
            A list of directories in your home directory that
            you want to link to persistent storage. You may optionally
            specify the linking method each directory should use.
          '';
        };

        files = mkOption {
          type = with types; listOf str;
          default = [ ];
          example = [ ".screenrc" ];
          description = ''
            A list of files in your home directory you want to
            link to persistent storage.
          '';
        };
      };
      config = mkIf cfg.enable {

        systemd.user.tmpfiles.rules = [
          "d  /persist/cache 0755 root root -"
          "d  /persist/cache/home 0755 ${host.username} users -"
          "d  /persist/cache/home/${host.username} 0755 ${host.username} users -"
        ];

        home.persistence."${host.persistDir}/cache" = {
          directories = [ ] ++ cfg.directories_cache;
        };

        home.persistence."${host.persistDir}/system" = {
          directories = [ ] ++ cfg.directories;
          files = [ ".screenrc" ] ++ cfg.files;
        };
      };
    };
}
