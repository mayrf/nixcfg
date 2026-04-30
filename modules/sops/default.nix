{ inputs, ... }:
{
  flake.modules.nixos.sops =
    { config, lib, ... }:
    let
      secretsPath = builtins.toString inputs.nix-secrets;
      user = config.host.username;
      ageKey =
        if config.host.isImpermanent then
          "/persist/system/home/${user}/.ssh/id_ed25519"
        else
          "/home/${user}/.ssh/id_ed25519";
    in
    {
      services.openssh.enable = true;
      sops = {
        defaultSopsFile = "${secretsPath}/secrets/secrets.yaml";
        defaultSopsFormat = "yaml";
        validateSopsFiles = false;
        age.sshKeyPaths = [ ageKey ];
        secrets."${user}/hashedPassword" = {
          neededForUsers = true;
        };
      };
    };
}
