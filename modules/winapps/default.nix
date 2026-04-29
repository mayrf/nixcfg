{ inputs, ... }:
{
  flake.modules.nixos.winapps =
    { pkgs, ... }:
    {
      nix.settings = {
        substituters = [ "https://winapps.cachix.org/" ];
        trusted-public-keys = [
          "winapps.cachix.org-1:HI82jWrXZsQRar/PChgIx1unmuEsiQMQq+zt05CD36g="
        ];
      };
      environment.systemPackages = [
        inputs.winapps.packages."${pkgs.system}".winapps
        inputs.winapps.packages."${pkgs.system}".winapps-launcher
      ];
    };
}
