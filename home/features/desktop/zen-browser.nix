{ config, lib, pkgs, inputs, ... }:
with lib;
let cfg = config.features.desktop.zen-browser;
in {

  imports = [
    inputs.zen-browser.homeModules.beta
    # or inputs.zen-browser.homeModules.twilight
    # or inputs.zen-browser.homeModules.twilight-official
  ];
  options.features.desktop.zen-browser.enable =
    mkEnableOption "zen-browser config";

  config = mkIf cfg.enable {

    features.impermanence.directories = [
      ".zen"
      # ".config/zen"
      ".cache/zen"
    ];

    stylix.targets.firefox.enable = false;
    stylix.targets.zen-browser.enable = false;

    programs.zen-browser = {
      enable = true;
      languagePacks = [ "en-GB" "de" ];
      # configPath = ".config/zen";
      configPath = ".zen";
      policies = {
        DisableAppUpdate = true;
        DisableTelemetry = true;
        # find more options here: https://mozilla.github.io/policy-templates/
      };
      # profiles."mayrf" = {
      #   isDefault = true;
      #   # search.default = "Brave Search";
      #   extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
      #     privacy-badger
      #     ublock-origin
      #   ];
      #   # search.engines = {
      #   #   nix-packages = {
      #   #     name = "Nix Packages";
      #   #     urls = [{
      #   #       template = "https://search.nixos.org/packages";
      #   #       params = [
      #   #         {
      #   #           name = "type";
      #   #           value = "packages";
      #   #         }
      #   #         {
      #   #           name = "query";
      #   #           value = "{searchTerms}";
      #   #         }
      #   #       ];
      #   #     }];

      #   #     icon =
      #   #       "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
      #   #     definedAliases = [ "@np" ];
      #   #   };

      #   #   nixos-wiki = {
      #   #     name = "NixOS Wiki";
      #   #     urls = [{
      #   #       template =
      #   #         "https://wiki.nixos.org/w/index.php?search={searchTerms}";
      #   #     }];
      #   #     iconMapObj."16" = "https://wiki.nixos.org/favicon.ico";
      #   #     definedAliases = [ "@nw" ];
      #   #   };

      #   #   bing.metaData.hidden = true;
      #   #   # google.metaData.alias = "@g"; # builtin engines only support specifying one additional alias

      #   #   brave-search = {
      #   #     name = "Brave Search";
      #   #     urls = [{
      #   #       template = "https://search.brave.com/search?q={searchTerms}";
      #   #     }];
      #   #     icon = "https://wiki.nixos.org/favicon.png";
      #   #     updateInterval = 24 * 60 * 60 * 1000; # every day
      #   #     definedAliases = [ "@nw" ];
      #   #   };

      #   # };
      # };
    };
  };
}
