{ ... }:
{
  flake.modules.nixos.docker =
    { pkgs, ... }:
    {
      virtualisation = {
        containers.enable = true;
        containers.containersConf.settings = {
          engine.compose_warning_logs = false;
        };
        docker.enable = true;
      };
      persistence.directories = [ "var/lib/containers/" ];
      security.pki.certificates = [ ];
      environment.systemPackages = with pkgs; [
        dive
        docker-compose
        docker-credential-helpers
      ];
    };
}
