{ config, pkgs, lib, hostSpec, inputs, ... }:
with lib;
let cfg = config.features.impermanence;
in {
  imports = [ inputs.impermanence.nixosModules.home-manager.impermanence ];
  options.features.impermanence = {
    enable = mkEnableOption "my impermanence config";
    directories = mkOption {
      # type = types.listOf
      #   (types.coercedTo types.str (directory: { inherit directory; })
      #     (submodule {
      #       options = {
      #         directory = mkOption {
      #           type = str;
      #           description = "The directory path to be linked.";
      #         };
      #         method = mkOption {
      #           type = types.enum [ "bindfs" "symlink" ];
      #           default = config.defaultDirectoryMethod;
      #           description = ''
      #             The linking method to be used for this specific
      #             directory entry. See
      #             <literal>defaultDirectoryMethod</literal> for more
      #             information on the tradeoffs.
      #           '';
      #         };
      #       };
      #     }));
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
      # type = types.listOf
      #   (types.coercedTo types.str (directory: { inherit directory; })
      #     (submodule {
      #       options = {
      #         directory = mkOption {
      #           type = str;
      #           description = "The directory path to be linked.";
      #         };
      #         method = mkOption {
      #           type = types.enum [ "bindfs" "symlink" ];
      #           default = config.defaultDirectoryMethod;
      #           description = ''
      #             The linking method to be used for this specific
      #             directory entry. See
      #             <literal>defaultDirectoryMethod</literal> for more
      #             information on the tradeoffs.
      #           '';
      #         };
      #       };
      # }));

      # type = lib.types.listOf li
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
      "d  /persist/cache/home 0755 ${hostSpec.username} users -"
      "d  /persist/cache/home/${hostSpec.username} 0755 ${hostSpec.username} users -"
    ];

    # home.persistence."${hostSpec.persistDir}/cache/home/${hostSpec.username}" =
    home.persistence."${hostSpec.persistDir}/cache" = 
      {
        # hideMounts = true;
        # allowOther = true;
        directories = [ ] ++ cfg.directories_cache;
      };

    # home.persistence."${hostSpec.persistDir}/system/home/${hostSpec.username}" =
    home.persistence."${hostSpec.persistDir}/system" =
      {
        # allowOther = true;
        directories = [ ] ++ cfg.directories;
        files = [ ".screenrc" ] ++ cfg.files;
      };
  };
}
