{ config, pkgs, ... }:
let
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
  features.impermanence.directories = [
    ".local/share/gem" # Ruby gems
  ];
  home.packages = with pkgs; [
    tmux
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
    copilot-language-server
    age
    statix
    nixfmt
    minio-client
    pandoc
    texlab
    texlive.combined.scheme-full
    zola
    hugo
    yaml-language-server
    rails-new
    rubyPackages_3_5.rails
  ];
}
