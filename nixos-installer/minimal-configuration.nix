{ lib, pkgs, myLib, device, ... }:
{
  imports = lib.flatten [
    (map myLib.relativeToRoot [
      "modules/common/host-spec.nix"
      "hosts/common/users/primary"
      "hosts/common/users/primary/nixos.nix"
      "hosts/common/global/ensure-config-repo.nix"
    ])
  ];

  hostSpec = {
    isMinimal = lib.mkForce true;
    username = "mayrf";
  };
  boot.initrd.systemd.enable = true;

  networking = {
    # configures the network interface(include wireless) via `nmcli` & `nmtui`
    # networkmanager.enable = true;
  };

  services = {
    # qemuGuest.enable = true;
    openssh = {
      enable = true;
      ports = [ 22 ];
      settings.PermitRootLogin = "yes";
    };
  };

  # allow sudo over ssh with yubikey
  security.pam = {
    sshAgentAuth.enable = true;
    services.sudo = {
      u2fAuth = true;
      sshAgentAuth = true;
    };
  };

  environment.systemPackages = builtins.attrValues {
    inherit (pkgs) wget curl rsync;
    # vim;
  };

  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    warn-dirty = false;
  };
  system.stateVersion = "25.05";
} // (if device != null then {
  fileSystems."/boot".options =
    [ "umask=0077" ]; # Removes permissions and security warnings.
  # boot.loader.efi.canTouchEfiVariables = true;
  # boot.loader.systemd-boot = {
  #   enable = true;
  #   # we use Git for version control, so we don't need to keep too many generations.
  #   configurationLimit = lib.mkDefault 3;
  #   # pick the highest resolution for systemd-boot's console.
  #   consoleMode = lib.mkDefault "max";
  # };
} else
  { })
