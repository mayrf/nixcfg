# Per-host configuration options
{ lib, ... }: {
  options.host = {
    username = lib.mkOption {
      type = lib.types.str;
      description = "The primary username of the host";
    };
    email = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      description = "Email addresses of the user";
    };
    work = lib.mkOption {
      default = { };
      type = lib.types.attrsOf lib.types.anything;
      description = "Work-related information";
    };
    networking = lib.mkOption {
      default = { };
      type = lib.types.attrsOf lib.types.anything;
      description = "Host-specific networking information";
    };
    domain = lib.mkOption {
      type = lib.types.str;
      description = "The domain of the host";
    };
    userFullName = lib.mkOption {
      type = lib.types.str;
      description = "Full name of the user";
    };
    handle = lib.mkOption {
      type = lib.types.str;
      description = "User handle (e.g. GitHub username)";
    };
    persistDir = lib.mkOption {
      type = lib.types.str;
      default = "/persist";
      description = "Persistence root directory (impermanence)";
    };
    flakeDir = lib.mkOption {
      type = lib.types.str;
      default = "$HOME/.config/nixcfg";
      description = "Path to the NixOS flake directory";
    };
    isImpermanent = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether this host uses impermanence";
    };
    isMinimal = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether this is a minimal host (no desktop)";
    };
  };
}
