{ pkgs, host, pkgs-stable, lib, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    ../common/global
    ../common/users/mayrf
    ../common/optional/pipewire.nix
    ../common/optional/lutris.nix
    ../common/linux.nix
    (import ./disko.nix { device = "/dev/nvme0n1"; })
  ];

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    binfmt.emulatedSystems = [ "aarch64-linux" "i686-linux" ];
  };

  services.ollama = {
    environmentVariables = { HSA_OVERRIDE_GFX_VERSION = "10.3.0"; };
    package = pkgs-stable.ollama;
    enable = true;
    acceleration = "rocm";
  };

  networking = {
    hostName = host; # Define your hostname.
  };
  hardware.ledger.enable = true;

  system.stateVersion = "24.05"; # Did you read the comment?

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
        "VirtualBox VMs"
        ".gnupg"
        ".ssh"
        ".nixops"
        ".thunderbird"
        ".local/share/keyrings"
        ".local/share/direnv"
        ".local/share/Steam"
        ".config/emacs"
        #{
        #  directory = ".local/share/Steam";
        #  method = "symlink";
        #}
      ];
      files = [ ".screenrc" ];
      #  allowOther = true;
    };
  };
  programs.fuse.userAllowOther = true;

}
