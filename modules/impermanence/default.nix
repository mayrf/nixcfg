{ inputs, ... }:
{
  flake.modules.nixos.impermanence =
    {
      lib,
      config,
      ...
    }:
    let
      cfg = config.persistence;
    in
    {
      imports = [
        # inputs.impermanence.nixosModules.impermanence
      ];

      config = lib.mkIf cfg.enable {
        fileSystems."/persist".neededForBoot = true;

        programs.fuse.userAllowOther = true;

        boot.tmp.cleanOnBoot = lib.mkDefault true;

        environment.persistence = {
          "/persist/userdata".users."${cfg.user}" = {
            directories = cfg.data.directories;
            files = cfg.data.files;
          };

          "/persist/usercache".users."${cfg.user}" = {
            directories = cfg.cache.directories;
            files = cfg.cache.files;
          };

          "/persist/cache" = {
            hideMounts = true;
            directories = [ "/var/lib/flatpak" ] ++ cfg.cache.directories;
          };

          "/persist/system" = {
            hideMounts = true;
            directories = [
              "/etc/nixos"
              "/var/log"
              # "/var/lib/bluetooth"
              "/var/lib/nixos"
              "/var/lib/libvirt"
              "/var/lib/systemd/coredump"
              "/etc/NetworkManager/system-connections"
              "/tmp"
              "/etc/ssh"
              "/root/.ssh"
              # {
              #   directory = "/var/lib/colord";
              #   user = "colord";
              #   group = "colord";
              #   mode = "u=rwx,g=rx,o=";
              # }
            ]
            ++ cfg.directories;

            files = [
              "/etc/machine-id"
              "/var/cache/locatedb"
              {
                file = "/var/keys/secret_file";
                parentDirectory = {
                  mode = "u=rwx,g=,o=";
                };
              }
            ]
            ++ cfg.files;

            users.${config.host.username} = {
              directories = [
                ".local/share/Steam"
                ".steam"
                ".ollama"
                ".local/share/containers"
                ".config/winapps"
              ];
            };
          };
        };

        boot.initrd.postDeviceCommands = lib.mkIf cfg.nukeRoot.enable (
          lib.mkAfter ''
            mkdir /btrfs_tmp
            mount /dev/${cfg.volumeGroup}/root /btrfs_tmp
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
          ''
        );

        systemd.tmpfiles.rules = [
          "d  /persist/cache 0755 root root -"
          "d  /persist/cache/home 0755 ${config.host.username} users -"
        ];
      };
    };
}
