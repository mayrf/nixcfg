{ ... }:
{
  flake.modules.homeManager.zathura =
    { ... }:
    {
      programs.zathura = {
        enable = true;
        options = { "selection-clipboard" = "clipboard"; };
      };
    };
}
