{ config, pkgs, inputs, outputs, lib, ... }:
let
  sopsHashedPasswordFile = lib.optionalString
    (!config.hostSpec.isMinimal && config.hostSpec.hostName != "iso")
    config.sops.secrets."${config.hostSpec.username}/hashedPassword".path;
  username = config.hostSpec.username;

  ifTheyExist = groups:
    builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
  pubKeys = lib.filesystem.listFilesRecursive ./keys;
in {

  users.mutableUsers =
    false; # Only allow declarative credentials; Required for password to be set via sops during system activation!

  users.users.${username} = {
    home = "/home/${username}";
    isNormalUser = true;
    description = "${username}";
    # initialPassword = "changeme";
    hashedPasswordFile = sopsHashedPasswordFile; # Blank if sops is not working.

    packages = [ pkgs.home-manager ];
    # These get placed into /etc/ssh/authorized_keys.d/<name> on nixos
    openssh.authorizedKeys.keys =
      lib.lists.forEach pubKeys (key: builtins.readFile key);

    extraGroups = lib.flatten [
      "wheel"
      (ifTheyExist [
        "flatpak"
        "input"
        "kvm"
        "qemu-libvirtd"
        "plugdev"
        "audio"
        "video"
        "docker"
        "git"
        "networkmanager"
        "network"
        "scanner" # for print/scan"
        "lp" # for print/scan"
        "libvirtd"
        "deluge"
      ])
    ];
  } // lib.optionalAttrs
    (config.hostSpec.isMinimal || config.hostSpec.hostName == "iso") {
      password = "nixos";
    };
  systemd.tmpfiles.rules = let
    user = config.users.users.${username}.name;
    group = config.users.users.${username}.group;
  in [
    "d /home/${username}/.ssh 0750 ${user} ${group} -"
    "d /home/${username}/.ssh/sockets 0750 ${user} ${group} -"
  ];

  home-manager = {
    extraSpecialArgs = {
      inherit outputs inputs;
      inherit (inputs.dotfiles-private) private;
      hostSpec = config.hostSpec;
    };
    users.${username}.imports = lib.flatten
      (lib.optional (!config.hostSpec.isMinimal) [
        ../../../home/${username}/${config.hostSpec.hostName}.nix
        inputs.impermanence.nixosModules.home-manager.impermanence
        inputs.nixvim.homeManagerModules.nixvim
        inputs.sops-nix.homeManagerModules.sops
      ] ++ (builtins.attrValues outputs.homeManagerModules));
  };

  # programs.nix-ld.enable = true;
  # programs.nix-ld.libraries = with pkgs;
  #   [
  #     # Add any missing dynamic libraries for unpackaged
  #     # programs here, NOT in environment.systemPackages
  #   ];
}
