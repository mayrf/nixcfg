{ ... }:
{
  flake.modules.homeManager.cliMedia =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        feh # Image Viewer
        sxiv
      ];
    };
}
