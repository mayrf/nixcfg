{
  inputs,
  self,
  lib,
  ...
}:
{
  flake.modules.nixos.emacs =
    {
      lib,
      config,
      ...
    }:
    {
      home-manager.sharedModules = [
        inputs.self.modules.homeManager.emacs
      ];

    };
  flake.modules.homeManager.emacs =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.dig ];
      programs.emacs = {
        enable = true;
        package = lib.mkForce (
          (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs: [
            epkgs.vterm
            epkgs.emacsql
            epkgs.pdf-tools
            epkgs.org
            epkgs.treesit-grammars.with-all-grammars
            epkgs.jinx
          ])
        );

      };
    };
}
