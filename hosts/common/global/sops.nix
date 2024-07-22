{ config, lib, pkgs, inputs, host, ... }:
let
  secretsPath = builtins.toString inputs.nix-secrets;
  ageKey = if host != "yttrium" then
    "/home/mayrf/.ssh/id_ed25519"
  else
    "/persist/system/home/mayrf/.ssh/id_ed25519";
in {
  sops = {
    defaultSopsFile = "${secretsPath}/secrets/secrets.yaml";
    defaultSopsFormat = "yaml";
    validateSopsFiles = false;

    age.sshKeyPaths = [ ageKey ];
    # secrets."wireguard_x220/public_key" = { };
    # secrets."wireguard_x220/private_key" = { };
    # secrets."wireguard_x220/endpoint" = { };
    secrets."mayrf/hashedPassword" = { neededForUsers = true; };
  };
}
