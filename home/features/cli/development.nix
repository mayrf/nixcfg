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
      devbox
      git
      git-crypt
      dbeaver-bin
      just
      python3
      compose2nix
      postgresql
      nerdctl
      httpyac
      sops
      age
      statix # Lints and suggestions for the nix programming language TODO: Use
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
