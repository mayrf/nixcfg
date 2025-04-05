# User config applicable only to nixos
{ inputs, config, lib, pkgs, ... }:
let
  hostSpec = config.hostSpec;
  ifTheyExist = groups:
    builtins.filter (group: builtins.hasAttr group config.users.groups) groups;

  #FIXME:(sops) sops-nix apparently works with darwin now so can probably move this, and password entries for user and root below to default.nix
  # Decrypt password to /run/secrets-for-users/ so it can be used to create the user

  sopsHashedPasswordFile = lib.optionalString
    (!config.hostSpec.isMinimal && config.hostSpec.hostName != "iso")
    config.sops.secrets."${config.hostSpec.username}/hashedPassword".path;
in {
  


  programs.zsh.enable = true;
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs;
    [
      # Add any missing dynamic libraries for unpackaged
      # programs here, NOT in environment.systemPackages
    ];

  security.pam.services = { swaylock = { }; };
  users.mutableUsers =
    false; # Only allow declarative credentials; Required for password to be set via sops during system activation!

  users.users.${hostSpec.username} = {
    home = "/home/${hostSpec.username}";
    isNormalUser = true;
    hashedPasswordFile = sopsHashedPasswordFile; # Blank if sops is not working.
    # initialPassword = "password";
    #  password = "nixos"; # This gets overridden if sops is working; it is only used on iso/nixos-installer

    packages = [ pkgs.home-manager ];

    extraGroups = lib.flatten [
      "wheel"
      (ifTheyExist [
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
  } // lib.optionalAttrs (!hostSpec.isMinimal) {
    shell =  pkgs.zsh;
  };

  # No matter what environment we are in we want these tools for root, and the user(s)
  programs.git.enable = true;

  # root's ssh key are mainly used for remote deployment, borg, and some other specific ops
  users.users.root = {
    shell = pkgs.zsh;
    hashedPasswordFile =
      config.users.users.${hostSpec.username}.hashedPasswordFile;
    password = lib.mkForce
      config.users.users.${hostSpec.username}.password; # This gets overridden if sops is working; it is only used if the hostSpec.hostName == "iso"

  #   // lib.optionalAttrs (hostSpec.isMinimal || hostSpec.hostName == "iso") {
  #     # This gets overridden if sops is working, only used on iso/nixos-installer
  #     password = "nixos";
    # root's ssh keys are mainly used for remote deployment.
    openssh.authorizedKeys.keys =
      config.users.users.${hostSpec.username}.openssh.authorizedKeys.keys;

  # # Create ssh sockets directory for controlpaths when homemanager not loaded (i.e. isMinimal)
  # systemd.tmpfiles.rules = let
  #   user = config.users.users.${hostSpec.username}.name;
  #   group = config.users.users.${hostSpec.username}.group;
  # in [ "d /home/${hostSpec.username}/.ssh/sockets 0750 ${user} ${group} -" ];

  # # No matter what environment we are in we want these tools
  # programs.zsh.enable = true;
  # environment.systemPackages = [ pkgs.just pkgs.rsync ];
  };
} // lib.optionalAttrs (inputs ? "home-manager") {

  # Setup p10k.zsh for root
  home-manager.users.root = lib.optionalAttrs (!hostSpec.isMinimal) {
    home.stateVersion = "25.05"; # Avoid error
    programs.zsh = {
      enable = true;
      # plugins = [{
      #   name = "powerlevel10k-config";
      #   src = lib.custom.relativeToRoot
      #     "home/${hostSpec.username}/common/core/zsh/p10k";
      #   file = "p10k.zsh";
      # }];
    };
  };
}
