{ ... }:
{
  flake.modules.homeManager.scripts =
    { config, ... }:
    {
      home.sessionPath = [
        "$HOME/.config/nixcfg/home/features/cli/scripts/bin"
      ];
    };
}
