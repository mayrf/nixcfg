{ inputs, ... }:
{
  perSystem = { pkgs, ... }: {
    packages.my-neovim =
      (inputs.nvf.lib.neovimConfiguration {
        pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
        modules = [ ../../pkgs/nvf_module ];
      }).neovim;
  };
}
