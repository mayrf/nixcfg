{ inputs, ... }:
{
  flake.modules.homeManager.zenBrowser =
    {
      config,
      lib,
      pkgs,
      inputs,
      ...
    }:
    {
      imports = [
        inputs.zen-browser.homeModules.beta
      ];

      features.impermanence.directories = [
        ".config/zen"
        ".cache/zen"
      ];

      programs.zen-browser = {
        enable = true;
        languagePacks = [
          "en-GB"
          "de"
        ];
        configPath = ".config/zen";
        policies = {
          DisableAppUpdate = true;
          DisableTelemetry = true;
        };
      };
    };
}
