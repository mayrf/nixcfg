{ config, lib, inputs, ... }:
with lib;
let
  cfg = config.features.sops;
  secretsPath = builtins.toString inputs.nix-secrets;
  user = config.hostSpec.username;
  ageKey = if config.hostSpec.isImpermanent then
    "/persist/system/home/${user}/.ssh/id_ed25519"
  else
    "/home/${user}/.ssh/id_ed25519";
in {
  options.features.sops.enable = mkEnableOption "sops config";
  config = mkIf cfg.enable {
    services.openssh.enable = true;
    sops = {
      defaultSopsFile = "${secretsPath}/secrets/secrets.yaml";
      defaultSopsFormat = "yaml";
      validateSopsFiles = false;
      age.sshKeyPaths = [ ageKey ];
      secrets."${user}/hashedPassword" = { neededForUsers = true; };
    };
  };
}
