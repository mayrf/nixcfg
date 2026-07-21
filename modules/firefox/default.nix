{ inputs, ... }:
{
  flake.modules.homeManager.firefox =
    {
      config,
      osConfig,
      pkgs,

      ...
    }:
    {
      imports = [
        # inputs.zen-browser.homeModules.beta
      ];

      # features.impermanence.directories = [
      #   ".config/zen"
      #   ".cache/zen"
      # ];

      programs.firefox = {
        enable = true;
        languagePacks = [
          "en-GB"
          "de"
        ];
        # configPath = ".config/zen";
        policies = {
          DisableAppUpdate = true;
          DisableTelemetry = true;
        };
        profiles.${osConfig.host.username} = {
          search = {
            # default = "brave";
            default = "ddg";


            force = true;
            engines = {
              nix-packages = {
                name = "Nix Packages";
                urls = [{
                  template = "https://search.nixos.org/packages";
                  params = [
                    { name = "type"; value = "packages"; }
                    { name = "query"; value = "{searchTerms}"; }
                  ];
                }];

                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = [ "@np" ];
              };
            };
          };
        };
      };
    };
}
