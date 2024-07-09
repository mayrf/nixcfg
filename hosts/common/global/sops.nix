{ config, lib, pkgs, inputs, ... }:
let secretsPath = builtins.toString inputs.nix-secrets;
in {
  sops = {
    defaultSopsFile = "${secretsPath}/secrets/secrets.yaml";
    defaultSopsFormat = "yaml";
    validateSopsFiles = false;

    age = { keyFile = "/home/mayrf/.config/sops/age/keys.txt"; };
    # secrets."wireguard_x220/public_key" = { };
    # secrets."wireguard_x220/private_key" = { };
    # secrets."wireguard_x220/endpoint" = { };
    secrets."mayrf/hashedPassword" = { };
  };
}
