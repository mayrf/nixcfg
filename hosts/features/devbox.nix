{ config, lib, devbox, ... }:
with lib;
let cfg = config.features.devbox;
in {
  options.features.devbox.enable = mkEnableOption "enable devbox";
  config = mkIf cfg.enable {
    # Enable flakes and nix-command (recommended for Devbox)
    nix.settings = {
      experimental-features = [ "nix-command" "flakes" ];

      # Add substituters for better package availability
      substituters = [
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org"
        "https://devenv.cachix.org"
      ];

      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
      ];

      # Allow unfree packages if needed
      allowed-unfree = true;

      # Keep build dependencies around for debugging
      keep-outputs = true;
      keep-derivations = true;
    };

    # Configure garbage collection
    nix.gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };

    # Auto-optimize store
    nix.settings.auto-optimise-store = true;

    # If you still want to use channels alongside flakes
    nix.channel.enable = true;

    # System packages that might be useful for development
    environment.systemPackages = with pkgs; [ devbox git curl wget ];
  };
}
