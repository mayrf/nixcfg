{ inputs, lib, pkgs, config, ... }:
let
  inherit (inputs.nix-colors) colorSchemes;
  inherit (inputs.nix-colors.lib-contrib { inherit pkgs; })
    colorschemeFromPicture nixWallpaperFromScheme;
in {
  imports = [ inputs.nix-colors.homeManagerModule ./shell.nix ./mimeApps.nix ];

  myvim.enable = true;
  myGhostty.enable = true;
  # myProton.enable = true;
  yazi.enable = true;

  home.file = {
    ".local/bin" = {
      source = ./scripts;
      recursive = true;
    };
  };

  nixpkgs = {
    overlays = [ inputs.emacs-overlay.overlays.emacs ];
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
      permittedInsecurePackages = [ ];

    };
  };

  nix = {
    package = lib.mkDefault pkgs.nix;
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
    git.enable = true;
  };

  colorscheme = lib.mkDefault colorSchemes.dracula;
  wallpaper = let
    largest = f: xs: builtins.head (builtins.sort (a: b: a > b) (map f xs));
    largestWidth = largest (x: x.width) config.monitors;
    largestHeight = largest (x: x.height) config.monitors;
  in lib.mkDefault (nixWallpaperFromScheme {
    scheme = config.colorscheme;
    width = largestWidth;
    height = largestHeight;
    logoScale = 4;
  });

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
