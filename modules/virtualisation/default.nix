{ ... }:
{
  flake.modules.homeManager.virtualisation =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.distrobox
      ];
      dconf.settings = {
        "org/virt-manager/virt-manager/connections" = {
          autoconnect = [ "qemu:///system" ];
          uris = [ "qemu:///system" ];
        };
      };
    };

  flake.modules.nixos.virtualisation =
    { ... }:
    {
      virtualisation.spiceUSBRedirection.enable = true;
      programs.virt-manager.enable = true;
      virtualisation.libvirtd = {
        enable = true;
        onShutdown = "suspend";
        onBoot = "ignore";
      };
    };
}
