{ config, lib, pkgs, ... }:

{
  sops = {
    defaultSopsFile = /home/mayrf/.config/nixcfg/sops/secrets/secrets.yaml;
    defaultSopsFormat = "yaml";
    validateSopsFiles = false;

    age = {
      sshKeyPaths = [ "/home/mayrf/.ssh/id_ed25519" ];
      keyFile = "/home/mayrf/.config/sops/age/keys.txt";
      generateKey = true;
    };

    secrets."wireguard_x220/public_key" = { };
    secrets."wireguard_x220/private_key" = { };
    secrets."wireguard_x220/endpoint" = { };

  };
}
