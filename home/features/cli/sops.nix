{ config, inputs, hostSpec, ... }:
let
  secretsPath = builtins.toString inputs.nix-secrets;
  ageKey = if hostSpec.isImpermanent then
    "/persist/system/home/${hostSpec.username}/.ssh/id_ed25519"
  else
    "/home/${hostSpec.username}/.ssh/id_ed25519";
in {
  imports = [ inputs.sops-nix.homeManagerModules.sops ];
  features.impermanence.directories = [ ".config/sops" ];
  sops = {
    defaultSopsFile = "${secretsPath}/secrets/secrets.yaml";
    defaultSopsFormat = "yaml";
    validateSopsFiles = false;
    age.sshKeyPaths = [ ageKey ];
  };
}
