{ inputs, ... }:
{
  perSystem = { pkgs, ... }: {
    packages = {
      auger = pkgs.callPackage ./_auger { };
      argocd-lovely-plugin = pkgs.callPackage ./_argocd-lovely-plugin { };
      my-neovim =
        (inputs.nvf.lib.neovimConfiguration {
          inherit pkgs;
          modules = [ ./_nvf_module ];
        }).neovim;
    };
  };
}
