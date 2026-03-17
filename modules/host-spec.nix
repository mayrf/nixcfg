# Specifications For Differentiating Hosts
{ config, pkgs, lib, ... }: {
  options.hostSpec = {
    username = lib.mkOption {
      type = lib.types.str;
      description = "The username of the host";
    };
    hostName = lib.mkOption {
      type = lib.types.str;
      description = "The hostname of the host";
    };
    sysStateVersion = lib.mkOption {
      type = lib.types.str;
      description = "The system state version of the system";
      default = "26.05";
    };
    email = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      description = "The email of the user";
    };
    # FIXME: Set an assert to make sure this is set if isWork is true
    work = lib.mkOption {
      default = { };
      type = lib.types.attrsOf lib.types.anything;
      description =
        "An attribute set of work-related information if isWork is true";
    };
    networking = lib.mkOption {
      default = { };
      type = lib.types.attrsOf lib.types.anything;
      description = "An attribute set of networking information";
    };
    domain = lib.mkOption {
      type = lib.types.str;
      description = "The domain of the host";
    };
    userFullName = lib.mkOption {
      type = lib.types.str;
      description = "The full name of the user";
    };
    handle = lib.mkOption {
      type = lib.types.str;
      description = "The handle of the user (eg: github user)";
    };
    home = lib.mkOption {
      type = lib.types.str;
      description = "The home directory of the user";
      default = let user = config.hostSpec.username;
      in if pkgs.stdenv.isLinux then "/home/${user}" else "/Users/${user}";
    };
    # FIXME: This should probably just switch to an impermenance option?
    persistDir = lib.mkOption {
      type = lib.types.str;
      description = "The folder to persist data if impermenance is enabled";
      default = "/persist";
    };
    flakeDir = lib.mkOption {
      type = lib.types.str;
      description = "The folder to persist data if impermenance is enabled";
      default = "$HOME/.config/nixcfg";
    };
    # TODO make the be dervived from other values by default
    isImpermanent = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Used to indicate a impermanence used by host";
    };
    isMinimal = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Used to indicate a minimal host";
    };
  };
}
