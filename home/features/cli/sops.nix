{ config, lib, inputs, hostSpec, ... }:
with lib;
let
  cfg = config.features.cli.sops;
  secretsPath = builtins.toString inputs.nix-secrets;
  ageKey = if hostSpec.isImpermanent then
    "/persist/system/home/${hostSpec.username}/.ssh/id_ed25519"
  else
    "/home/${hostSpec.username}/.ssh/id_ed25519";
in {
  options.features.cli.sops.enable = mkEnableOption "my sops user config";
  config = mkIf cfg.enable {
    # services.openssh.enable = true;
    sops = {
      defaultSopsFile = "${secretsPath}/secrets/secrets.yaml";
      defaultSopsFormat = "yaml";
      validateSopsFiles = false;
      age.sshKeyPaths = [ ageKey ];
    };
  };
}
