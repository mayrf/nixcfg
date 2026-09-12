{ ... }:
{
  flake.modules.homeManager.teams =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [ teams ];
    };
}
