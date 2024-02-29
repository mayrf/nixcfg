{ inputs, lib, pkgs, config, outputs, ... }:
let
  inherit (inputs.nix-colors) colorSchemes;
  inherit (inputs.nix-colors.lib-contrib { inherit pkgs; })
    colorschemeFromPicture nixWallpaperFromScheme;
in {
  imports = [
    inputs.nix-colors.homeManagerModule
    ./git
    ./shell.nix
    ./mimeApps.nix
    ../features/editors/nvim.nix
  ] ++ (builtins.attrValues outputs.homeManagerModules);

  nixpkgs = {
    overlays = builtins.attrValues outputs.overlays;
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
      permittedInsecurePackages = [ "electron-25.9.0" ];
    };
  };

  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = [ "nix-command" "flakes" "repl-flake" ];
      warn-dirty = false;
      keep-outputs = true;
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

  # For virtualisation
  # dconf.settings = {
  #   "org/virt-manager/virt-manager/connections" = {
  #     autoconnect = [ "qemu:///system" ];
  #     uris = [ "qemu:///system" ];
  #   };
  # };
}
