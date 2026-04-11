{ inputs, self, ... }:
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
    };
}
