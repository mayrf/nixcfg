{ ... }:
{
  flake.modules.homeManager.alacritty =
    { ... }:
    {
      programs = {
        alacritty = {
          enable = true;
          settings = {
            window = {
              padding = {
                x = 15;
                y = 15;
              };
            };
            scrolling = {
              history = 10000;
              multiplier = 3;
            };
          };
        };
      };
    };
}
