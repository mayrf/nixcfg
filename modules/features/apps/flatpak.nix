{ inputs, ...}: {
  flake.nixosModules.flatpak = { pkgs, ...}: {
    services.flatpak.enable = true;
    xdg.portal.enable = true;
    xdg.portal.config.common.default = "*";
  };
}
