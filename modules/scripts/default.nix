{ ... }:
{
  flake.modules.homeManager.scripts =
    { config, ... }:
    {
      home.sessionPath = [
        "$HOME/.config/nixcfg/modules/scripts/bin"
      ];
    };
}
