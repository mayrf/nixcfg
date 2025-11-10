{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.cli.lf;
in {
  options.features.cli.lf.enable = mkEnableOption "my lf user config";
  config = mkIf cfg.enable {
    xdg.configFile = {
      "lf/lfrc".source = ./lfrc;
      "lf/icons".source = ./icons;
      "lf/colors".source = ./colors;
    };

    home.packages = with pkgs; [
      # lf dependencies
      ctpv
      ueberzugpp
      ffmpeg_6-full
      jq
      poppler-utils
      bat
    ];

    programs.lf = { enable = true; };
  };
}
