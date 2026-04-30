{ inputs, ... }:
{
  flake.modules.homeManager.zenBrowser =
    { config, lib, pkgs, inputs, ... }:
    {
      imports = [
        inputs.zen-browser.homeModules.beta
      ];

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
