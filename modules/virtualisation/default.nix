{ ... }:
{
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
