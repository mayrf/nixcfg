{ pkgs, ... }: {
  fontProfiles = {
    enable = true;
    monospace = {
      family = "FiraCode Nerd Font";
      package = pkgs.unstable.nerd-fonts.fira-code;
    };
    regular = {
      family = "Fira Sans";
      package = pkgs.fira;
    };
  };
}
