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
        key_bindings = [{
          key = "Return";
          mods = "Alt|Shift";
          action = "SpawnNewInstance";
        }];
      };
    };
  };
}
