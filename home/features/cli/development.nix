{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.cli.development;
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
    ];
  };
}
