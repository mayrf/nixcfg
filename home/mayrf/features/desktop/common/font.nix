{ pkgs, unstable, ... }: {
  fontProfiles = {
    enable = true;
    monospace = {
      family = "FiraCode Nerd Font";
      package = unstable.nerd-fonts.fira-code;
    };
    regular = {
      family = "Fira Sans";
      package = pkgs.fira;
    };
  };
}
