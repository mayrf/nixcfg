{self, ...}: {
  flake.modules.nixos.general = {
    pkgs,
    config,
    lib,
    ...
  }: let
    ifTheyExist = groups: builtins.filter (group: builtins.hasAttr group config.users.groups) groups;
    pubKeys = lib.filesystem.listFilesRecursive ../common/keys;
    useSops = config.sops.secrets ? "${config.host.username}/hashedPassword";
  in {
    imports = [
      self.modules.nixos.nix
    ];

    # User
    users.mutableUsers = !useSops;
    users.users.${config.host.username} =
      {
        home = "/home/${config.host.username}";
        isNormalUser = true;
        # description = "${username}";
        description = "${config.host.username}'s account";
        shell = pkgs.zsh;
        packages = [pkgs.home-manager];
        openssh.authorizedKeys.keys = lib.lists.forEach pubKeys (key: builtins.readFile key);
        extraGroups = lib.flatten [
          "wheel"
          (ifTheyExist [
            "flatpak"
            "docker"
            "input"
            "kvm"
            "qemu-libvirtd"
            "plugdev"
            "audio"
            "video"
            "git"
            "networkmanager"
            "network"
            "scanner"
            "lp"
            "libvirtd"
            "deluge"
          ])
        ];
      }
      // lib.optionalAttrs (config.host.isMinimal) {
        password = "nixos";
      }
      // lib.optionalAttrs useSops {
        hashedPasswordFile = config.sops.secrets."${config.host.username}/hashedPassword".path;
      }
      // lib.optionalAttrs (!config.host.isMinimal && !useSops) {
        initialPassword = "changeme";
      };
  };
}
