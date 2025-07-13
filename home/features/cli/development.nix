{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.features.cli.development;

  yamlLsConfig = pkgs.writeText "yaml-ls-config.json" ''
    {
      "yaml": {
        "kubernetes": "*.yaml",
        "schemas": {
          "kubernetes": "*.yaml",
          "https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.28.0/all.json": "*.k8s.yaml"
        },
        "completion": true,
        "hover": true,
        "validate": true,
        "format": {
          "enable": true
        }
      }
    }
  '';
in {
  options.features.cli.development.enable =
    mkEnableOption "enable development cli programs";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      tmux
      nodejs
      yarn
      devbox
      devenv
      git
      git-crypt
      just
      python3
      python313Packages.debugpy
      compose2nix
      postgresql
      nerdctl
      httpyac
      sops
      copilot-language-server-fhs
      age
      statix # Lints and suggestions for the nix programming language TODO: Use
      nixfmt-classic
      # (python3.withPackages (ps: with ps; [ jupyter ]))
      minio-client
      # TODO add Scripts like:
      # pandoc -f markdown -t org -o ${md%.*}.org ${md};
      pandoc
      # Dev tools
      texlab
      texlive.combined.scheme-full
      zola
      hugo
      yaml-language-server
    ];

    # User-specific config
    # home.file.".config/yaml-language-server/config.json".source = yamlLsConfig;

    # # Set environment variable
    # home.sessionVariables = {
    #   YAML_LANGUAGE_SERVER_CONFIG =
    #     "${config.home.homeDirectory}/.config/yaml-language-server/config.json";
    # };
  };
}
