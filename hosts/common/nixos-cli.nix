{ inputs, ... }: {

  programs.nixos-cli = {
    enable = true;
    settings = {
      # Whatever settings desired.
      option.min_score = 2;
    };
  };
  nix.settings = {
    substituters = [ "https://watersucks.cachix.org" ];
    trusted-public-keys = [
      "watersucks.cachix.org-1:6gadPC5R8iLWQ3EUtfu3GFrVY7X6I4Fwz/ihW25Jbv8="
    ];
  };
}
