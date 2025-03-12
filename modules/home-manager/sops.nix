{ config, lib, host, inputs, ... }:
with lib;
let cfg = config.mySops;
  secretsPath = builtins.toString inputs.nix-secrets;
  ageKey = if host != "yttrium" then
    "/home/mayrf/.ssh/id_ed25519"
  else
    "/persist/system/home/mayrf/.ssh/id_ed25519";
in {
  options.mySops = { enable = mkEnableOption "my sops user config"; };
  config = mkIf cfg.enable {

    sops = {
      
        defaultSopsFile = "${secretsPath}/secrets/secrets.yaml";
        defaultSopsFormat = "yaml";
        validateSopsFiles = false;

        age.sshKeyPaths = [ ageKey ];
    };
  };
}
