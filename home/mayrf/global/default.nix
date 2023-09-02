{ inputs, lib, pkgs, config, outputs, ... }:
let
  inherit (inputs.nix-colors) colorSchemes;
  inherit (inputs.nix-colors.lib-contrib { inherit pkgs; }) colorschemeFromPicture nixWallpaperFromScheme;
in
{
  imports = [
    inputs.nix-colors.homeManagerModule
    ./git
    ./home.nix
    ./shell.nix
    ./mimeApps.nix
    ../features/editors/nvim.nix
  ] ++ (builtins.attrValues outputs.homeManagerModules);

  nixpkgs = {
    overlays = builtins.attrValues outputs.overlays;
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
      permittedInsecurePackages = [
        "electron-12.2.3"
      ];
    };
  };

  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = [ "nix-command" "flakes" "repl-flake" ];
      warn-dirty = false;
    };
  };


  systemd.user.startServices = "sd-switch";

  programs = {
    home-manager.enable = true;
    git.enable = true;
  };

  home = {
    username = lib.mkDefault "mayrf";
    homeDirectory = lib.mkDefault "/home/${config.home.username}";
    stateVersion = lib.mkDefault "23.05";
    sessionPath = [ "$HOME/.local/bin" ];

    # persistence = {
    #   "/persist/home/mayrf" = {
    #     directories = [
    #       "Documents"
    #       "Downloads"
    #       "Pictures"
    #       "Videos"
    #       ".local/bin"
    #     ];
    #     allowOther = true;
    #   };
    # };
  };

  colorscheme = lib.mkDefault colorSchemes.dracula;
  wallpaper =
    let
      largest = f: xs: builtins.head (builtins.sort (a: b: a > b) (map f xs));
      largestWidth = largest (x: x.width) config.monitors;
      largestHeight = largest (x: x.height) config.monitors;
    in
    lib.mkDefault (nixWallpaperFromScheme
      {
        scheme = config.colorscheme;
        width = largestWidth;
        height = largestHeight;
        logoScale = 4;
      });

  home.file.".colorscheme".text = config.colorscheme.slug;

  fonts.fontconfig.enable = true;

  services = {
    nextcloud-client = {
      enable = true;
      startInBackground = true;
    };
    gpg-agent = {
      enable = true;
      pinentryFlavor = "qt";
    };
  };

  xdg = {
    enable = true;
    configHome = "${config.home.homeDirectory}/.config";
    cacheHome = "${config.home.homeDirectory}/.cache";
    dataHome = "${config.home.homeDirectory}/.local/share";
    stateHome = "${config.home.homeDirectory}/.local/state";
  };
}
