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
      ".config/zen"
      ".cache/zen"
    ];

    stylix.targets.firefox.enable = false;
    stylix.targets.zen-browser.enable = false;

    programs.zen-browser = {
      enable = true;
      languagePacks = [ "en-GB" "de" ];
      configPath = ".config/zen";
      policies = {
        DisableAppUpdate = true;
        DisableTelemetry = true;
      };
    };
  };
}
