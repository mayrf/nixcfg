{ config, pkgs, lib, ... }:
with lib;
let cfg = config.mymodules.impermanence;
in {
  options.mymodules.impermanence = {
    enable = mkEnableOption "my impermanence config";
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

    fileSystems."/persist".neededForBoot = true;
    environment.persistence."/persist/system" = {
      hideMounts = true;
      directories = [
        "/var/log"
        "/var/lib/bluetooth"
        "/var/lib/nixos"
        "/var/lib/systemd/coredump"
        # "/var/lib/ollama"
        {
          directory = "/var/lib/private";
          mode = "u=rwx,g=,o=";
        }
        {
          directory = "/var/lib/private/ollama";
          mode = "0700";
        }
        "/etc/NetworkManager/system-connections"
        {
          directory = "/etc/nixos";
          user = "mayrf";
          group = "users";
          mode = "0777";
        }
        {
          directory = "/var/lib/colord";
          user = "colord";
          group = "colord";
          mode = "u=rwx,g=rx,o=";
        }
      ];
      files = [
        "/etc/machine-id"
        "/var/lib/swapfile"
        {
          file = "/var/keys/secret_file";
          parentDirectory = { mode = "u=rwx,g=,o="; };
        }
      ];

      users.mayrf = {
        directories = [
          "Downloads"
          "Music"
          "Pictures"
          "Documents"
          "Videos"
          "code"
          "cloud"
          "VirtualBox VMs"
          ".gnupg"
          ".librewolf"
          ".ssh"
          ".nixops"
          ".thunderbird"
          ".local/share/keyrings"
          ".local/share/direnv"
          ".local/share/Steam"
          ".steam"
          ".local/share/oterm"
          ".config/emacs"
          ".config/Signal"
          ".config/git"
          ".config/Nextcloud"
          ".config/keepassxc"
          ".config/BraveSoftware"
          ".config/sops"
          ".ollama"
          ".cache/keepassxc"

          #{
          #  directory = ".local/share/Steam";
          #  method = "symlink";
          #}
        ];
        files = [ ".zsh_history" ".git-credentials" ];
        # allowOther = true;
      };
    };
    programs.fuse.userAllowOther = true;
  };
}
