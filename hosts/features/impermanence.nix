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
      #   type = listOf (coercedTo str (f: { file = f; }) rootFile);
      default = [ ];
      example = [ "/etc/machine-id" "/etc/nix/id_rsa" ];
      description = ''
        Files that should be stored in persistent storage.
      '';
    };
  };
  config = mkIf cfg.enable {

    # boot.initrd.systemd = {
    #   enable =
    #     true; # this enabled systemd support in stage1 - required for the below setup
    #   # services.rollback = {
    #   #   description = "Rollback BTRFS root subvolume to a pristine state";
    #   #   wantedBy = [ "initrd.target" ];

    #   #   # LUKS/TPM process. If you have named your device mapper something other
    #   #   # than 'enc', then @enc will have a different name. Adjust accordingly.
    #   #   after = [ "systemd-cryptsetup@crypted.service" ];

    #   #   # Before mounting the system root (/sysroot) during the early boot process
    #   #   before = [ "sysroot.mount" ];

    #   #   unitConfig.DefaultDependencies = "no";
    #   #   serviceConfig.Type = "oneshot";
    #   #   script = ''
    #   #     mkdir -p /mnt

    #   #     # We first mount the BTRFS root to /mnt
    #   #     # so we can manipulate btrfs subvolumes.
    #   #     mount -o subvol=/ /dev/mapper/enc /mnt

    #   #     # While we're tempted to just delete /root and create
    #   #     # a new snapshot from /root-blank, /root is already
    #   #     # populated at this point with a number of subvolumes,
    #   #     # which makes `btrfs subvolume delete` fail.
    #   #     # So, we remove them first.
    #   #     #
    #   #     # /root contains subvolumes:
    #   #     # - /root/var/lib/portables
    #   #     # - /root/var/lib/machines

    #   #     btrfs subvolume list -o /mnt/root |
    #   #       cut -f9 -d' ' |
    #   #       while read subvolume; do
    #   #         echo "deleting /$subvolume subvolume..."
    #   #         btrfs subvolume delete "/mnt/$subvolume"
    #   #       done &&
    #   #       echo "deleting /root subvolume..." &&
    #   #       btrfs subvolume delete /mnt/root
    #   #     echo "restoring blank /root subvolume..."
    #   #     btrfs subvolume snapshot /mnt/root-blank /mnt/root

    #   #     # Once we're done rolling back to a blank snapshot,
    #   #     # we can unmount /mnt and continue on the boot process.
    #   #     umount /mnt
    #   #   '';
    #   # };
    # };

    # this disables systemd support in stage1 - required for the below setup
    boot.initrd.systemd.enable = false;
    boot.initrd.postResumeCommands = lib.mkAfter ''
      mkdir /btrfs_tmp
      mount /dev/root_vg/root /btrfs_tmp
      if [[ -e /btrfs_tmp/root ]]; then
          mkdir -p /btrfs_tmp/old_roots
          timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%-d_%H:%M:%S")
          if [[ ! -e /btrfs_tmp/old_roots/$timestamp ]]; then
            mv /btrfs_tmp/root "/btrfs_tmp/old_roots/$timestamp"
          else
            btrfs subvolume delete /btrfs_tmp/root
          fi
      fi

      delete_subvolume_recursively() {
          btrfs subvolume delete -R "$1"
          # IFS=$'\n'
          # for i in $(btrfs subvolume list -o "$1" | cut -f 9- -d ' '); do
          #     delete_subvolume_recursively "/btrfs_tmp/$i"
          # done
          # btrfs subvolume delete "$1"
      }

      for i in $(find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +30); do
          delete_subvolume_recursively "$i"
      done

      btrfs subvolume create /btrfs_tmp/root
      umount /btrfs_tmp
    '';

    fileSystems.${config.hostSpec.persistDir}.neededForBoot = true;

    systemd.tmpfiles.rules = [
      "d  /persist/cache 0755 root root -"
      "d  /persist/cache/home 0755 ${config.hostSpec.username} users -"
      "d  /persist/cache/home/${config.hostSpec.username} 0755 ${config.hostSpec.username} users -"
    ];

    environment.persistence."${config.hostSpec.persistDir}/cache" = {
      hideMounts = true;
      directories = [ "/var/lib/flatpak" ] ++ cfg.directories_cache;
    };

    environment.persistence."${config.hostSpec.persistDir}/system" = {
      hideMounts = true;
      directories = [
        "/var/log"
        "/var/lib/nixos"
        "/var/lib/libvirt"
        "/var/lib/systemd/coredump"
        "/etc/NetworkManager/system-connections"
        "/etc/ssh"
        "/root/.ssh"

        # Cups
        {
          directory = "/var/lib/colord";
          user = "colord";
          group = "colord";
          mode = "u=rwx,g=rx,o=";
        }
      ] ++ cfg.directories;

      files = [
        "/etc/machine-id"
        # "/var/lib/swapfile"
        # Locate
        "/var/cache/locatedb"
        {
          file = "/var/keys/secret_file";
          parentDirectory = { mode = "u=rwx,g=,o="; };
        }
      ];

      users.${config.hostSpec.username} = {
        directories = [
          ".local/share/Steam"
          ".steam"
          ".ollama"
          ".local/share/containers"
          ".config/winapps"
        ];
      };
    };
    programs.fuse.userAllowOther = true;
  };
}
