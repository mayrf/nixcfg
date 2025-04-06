{ config, lib, pkgs, inputs, ... }:
let
  secretsPath = builtins.toString inputs.nix-secrets;
  user = config.hostSpec.username;

  ageKey = if config.hostSpec.isImpermanent then
    "/home/${user}/.ssh/id_ed25519"
  else
    "/persist/system/home/${user}/.ssh/id_ed25519";
in {
  sops = {
    defaultSopsFile = "${secretsPath}/secrets/secrets.yaml";
    defaultSopsFormat = "yaml";
    validateSopsFiles = false;
    age.sshKeyPaths = [ ageKey ];
    secrets."${user}/hashedPassword" = { neededForUsers = true; };
  };
}
