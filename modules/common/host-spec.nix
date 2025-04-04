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
    persistDirRoot = lib.mkOption {
      type = lib.types.str;
      description = "The folder to persist root data if impermenance is enabled";
      default = "/persist";
    };
    persistDirNoBak = lib.mkOption {
      type = lib.types.str;
      description = "The folder to persist data which should not be backed up if impermenance is enabled";
      default = "/persist/no_bak";
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
    isProduction = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Used to indicate a production host";
    };
    isServer = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Used to indicate a server host";
    };
    isWork = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Used to indicate a host that uses work resources";
    };
    # Sometimes we can't use pkgs.stdenv.isLinux due to infinite recursion
    isDarwin = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Used to indicate a host that is darwin";
    };
    useYubikey = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Used to indicate if the host uses a yubikey";
    };
    voiceCoding = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Used to indicate a host that uses voice coding";
    };

    # FIXME: Maybe make this display sub options or something later
    hdr = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Used to indicate a host that uses HDR";
    };
    scaling = lib.mkOption {
      type = lib.types.str;
      default = "1";
      description =
        "Used to indicate what scaling to use. Floating point number";
    };
  };
}
