{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.features.impermanence;
  rootFile = submodule [
    commonOpts
    fileOpts
    ({ config, ... }: {
      parentDirectory = mkDefault (defaultPerms // rec {
        directory = dirOf config.file;
        dirPath = directory;
        inherit (config) persistentStoragePath;
        inherit defaultPerms;
      });
      filePath = mkDefault config.file;
    })
  ];
in {
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
      # }));

      # type = lib.types.listOf li
      default = [ ];
      example = [
        "Downloads"
        "Music"
        "Pictures"
        "Documents"
        "Videos"
        "VirtualBox VMs"
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
      type = listOf (coercedTo str (f: { file = f; }) rootFile);
      default = [ ];
      example = [ "/etc/machine-id" "/etc/nix/id_rsa" ];
      description = ''
        Files that should be stored in persistent storage.
      '';
    };
  };
  config = mkIf cfg.enable {
    boot.initrd.postDeviceCommands = lib.mkAfter ''
      mkdir /btrfs_tmp
      mount /dev/root_vg/root /btrfs_tmp
      if [[ -e /btrfs_tmp/root ]]; then
          mkdir -p /btrfs_tmp/old_roots
          timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%-d_%H:%M:%S")
          mv /btrfs_tmp/root "/btrfs_tmp/old_roots/$timestamp"
      fi

      delete_subvolume_recursively() {
          IFS=$'\n'
          for i in $(btrfs subvolume list -o "$1" | cut -f 9- -d ' '); do
              delete_subvolume_recursively "/btrfs_tmp/$i"
          done
          btrfs subvolume delete "$1"
      }

      for i in $(find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +30); do
          delete_subvolume_recursively "$i"
      done

      btrfs subvolume create /btrfs_tmp/root
      umount /btrfs_tmp
    '';

    fileSystems.${config.hostSpec.persistDir}.neededForBoot = true;
    environment.persistence."${config.hostSpec.persistDir}/system" = {
      hideMounts = true;
      directories = [
        "/var/log"
        "/var/lib/bluetooth"
        "/var/lib/nixos"
        "/var/lib/systemd/coredump"
        "/etc/NetworkManager/system-connections"
        "/etc/ssh"

        # Cups

        "/var/lib/flatpak"

        {
          directory = "/var/lib/colord";
          user = "colord";
          group = "colord";
          mode = "u=rwx,g=rx,o=";
        }

        {
          directory = "/etc/nixos";
          user = "mayrf";
          group = "users";
          mode = "0777";
        }

        "/var/lib/private/open-webui"
        {
          directory = "/var/lib/private";
          mode = "u=rwx,g=,o=";
        }
        {
          directory = "/var/lib/private/ollama";
          mode = "0700";
        }
      ] ++ cfg.directories;

      files = [
        "/etc/machine-id"
        "/var/lib/swapfile"
        # Locate
        "/var/cache/locatedb"
        {
          file = "/var/keys/secret_file";
          parentDirectory = { mode = "u=rwx,g=,o="; };
        }
      ];

      users.${config.hostSpec.username} = {
        directories = [
          "VirtualBox VMs"
          ".local/share/Steam"
          ".steam"
          ".ollama"
          # private stuff
          ".sparrow"
          ".local/share/Bisq2"
        ];
     };
    };
    programs.fuse.userAllowOther = true;
  };
}
