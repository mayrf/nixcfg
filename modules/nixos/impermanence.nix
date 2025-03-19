{ config, pkgs, lib, configVars, ... }:
with lib;
let
  cfg = config.mymodules.impermanence;
  persistenceDir = lib.types.option
    inputs.impermanence.persistence."/persist/no_bak".directories;
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

    fileSystems.${configVars.persistDirRoot}.neededForBoot = true;
    environment.persistence.${configVars.persistDir} = {
      hideMounts = true;
      directories = [
        "/var/log"
        "/var/lib/bluetooth"
        "/var/lib/nixos"
        "/var/lib/systemd/coredump"
        "/etc/NetworkManager/system-connections"

        # Cups
        "/var/cache/cups"
        "/lib/lib/cups"

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
      ];

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

      users.${configVars.username} = {
        directories = [
          "Downloads"
          "Music"
          "Pictures"
          "Documents"
          "playground"
          "Videos"
          "code"
          "cloud"
          ".ssh"
          ".gnupg"
          # docker / distrobox
          ".local/share/containers"

          "VirtualBox VMs"

          ".librewolf"

          ".thunderbird"

          ".local/share/keyrings"

          ".local/share/direnv"

          ".local/share/Steam"

          ".local/share/fonts"

          ".local/share/nautilus"
          # For nautilus bookmarks
          # ".config/gtk-3.0/bookmarks"
          ".config/gtk-3.0"

          ".local/share/gnucash"

          ".local/share/Anki2"

          ".steam"

          ".local/share/oterm"

          ".config/emacs-doom"

          ".config/Signal"

          ".config/git"

          ".config/Nextcloud"

          ".config/keepassxc"

          ".config/sops"
          ".config/calibre"

          ".ollama"

          ".cache/keepassxc"

          # Brave
          ".config/BraveSoftware"
          ".cache/BraveSoftware"
          ".local/share/kwalletd/"

          ".config/FreeTube"

          ".local/share/Nextcloud/"

          ".config/libreoffice"

          #{
          #  directory = ".local/share/Steam";
          #  method = "symlink";
          #}
          # private stuff
          ".sparrow"
          ".local/share/Bisq2"
        ];
        files = [
          ".zsh_history"
          ".git-credentials"

          ".kube/config"
          # Brave
          ".config/kwalletrc"
        ];
        # allowOther = true;
      };
    };
    programs.fuse.userAllowOther = true;
  };
}
