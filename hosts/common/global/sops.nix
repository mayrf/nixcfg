{inputs, config, lib, pkgs, user, ... }:

let
  secretsDirectory = builtins.toString inputs.nix-secrets;
  secretsFile = "${secretsDirectory}/secrets.yaml";
  homeDirectory = "/home/${user}";
{
  sops = {
    defaultSopsFile = "{secretsFile}";
    # defaultSopsFormat = "yaml";
    validateSopsFiles = false;

    age = {
      sshKeyPaths = [ "/home/mayrf/.ssh/id_ed25519" ];
      # keyFile = "/home/mayrf/.config/sops/age/keys.txt";
      # generateKey = true;
    };

    secrets."wireguard_x220/public_key" = { };
    secrets."wireguard_x220/private_key" = { };
    secrets."wireguard_x220/endpoint" = { };

  };
}
