{ inputs, lib, pkgs, config, outputs, hostSpec, ... }:
let inherit (inputs.nix-colors) colorSchemes;
in {
  imports = [
    inputs.nix-colors.homeManagerModule
    ../../modules/common/host-spec.nix
    ./mime-apps.nix
  ];

  services = {
    gpg-agent = {
      enable = true;
      pinentry.package = pkgs.pinentry-qt;
    };
  };

  home = {
    username = hostSpec.username;
    homeDirectory = lib.mkDefault "/home/${hostSpec.username}";
    stateVersion = hostSpec.sysStateVersion; # Did you read the comment?
    sessionPath = [ "$HOME/.local/bin" ];
  };

  features.impermanence.directories = [

    ".ssh"
    ".gnupg"
    ".local/share/keyrings"
    "Downloads"
    "Music"
    "Pictures"
    "Documents"
    "playground"
    "Videos"
    "code"
    "cloud"
    ".local/share/fonts"
  ];

  home.file = {
    ".local/bin" = {
      source = ./scripts;
      recursive = true;
    };
  };

  nixpkgs = {
    overlays = [
      inputs.emacs-overlay.overlays.emacs
      outputs.overlays.additions
      outputs.overlays.stable-packages
      outputs.overlays.unstable-packages
      inputs.nur.overlays.default
    ];
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
      permittedInsecurePackages = [ ];

    };
  };

  nix = {
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    settings = {
      warn-dirty = false;
      keep-outputs = true;
      experimental-features = [ "nix-command" "flakes" ];
      substituters =
        [ "https://hyprland.cachix.org" "https://nix-community.cachix.org/" ];
      trusted-public-keys = [
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };

    registry = {
      "mytemplates" = {
        from = {
          # type = "path";
          # path = "~/.config/nixcfg";
          # id = "nixpkgs";
          id = "mytemplates";
          type = "indirect";
        };
        to = {
          # path = "/home/mayrf/.config/nixcfg";
          path = "${config.xdg.configHome}/nixcfg";
          type = "path";
        };
      };
    };
  };
  systemd.user.startServices = "sd-switch";

  programs = {
    home-manager.enable = true;
  };

  home.file.".colorscheme".text = config.colorscheme.slug;

  fonts.fontconfig.enable = true;

  xdg = {
    enable = true;
    configHome = "${config.home.homeDirectory}/.config";
    cacheHome = "${config.home.homeDirectory}/.cache";
    dataHome = "${config.home.homeDirectory}/.local/share";
    stateHome = "${config.home.homeDirectory}/.local/state";
  };

}
