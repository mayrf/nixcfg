# User config applicable only to nixos
{ inputs, config, lib, pkgs, ... }:
{
  # root's ssh key are mainly used for remote deployment, borg, and some other specific ops
  users.users.root = {
    hashedPasswordFile =
      config.users.users.${config.hostSpec.username}.hashedPasswordFile;
    password = lib.mkForce
      config.users.users.${config.hostSpec.username}.password; # This gets overridden if sops is working; it is only used if the hostSpec.hostName == "iso"

    # root's ssh keys are mainly used for remote deployment.
    openssh.authorizedKeys.keys =
      config.users.users.${config.hostSpec.username}.openssh.authorizedKeys.keys;

  # environment.systemPackages = [ pkgs.just pkgs.rsync ];
  };
} // lib.optionalAttrs (inputs ? "home-manager") {
  home-manager.users.root = lib.optionalAttrs (!config.hostSpec.isMinimal) {
    home.stateVersion = config.hostSpec.sysStateVersion; # Avoid error
  };
}
