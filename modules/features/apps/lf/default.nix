{ ... }:
{
  flake.modules.homeManager.lf =
    { config, pkgs, ... }:
    {
      xdg.configFile = {
        "lf/lfrc".source = ./lfrc;
        "lf/icons".source = ./icons;
        "lf/colors".source = ./colors;
      };

      home.packages = with pkgs; [
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
