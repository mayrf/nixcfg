{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.cli.git;
in {
  options.features.cli.git.enable = mkEnableOption "my git user config";

  config = mkIf cfg.enable {

    features.impermanence.directories = [ ".config/git" ];
    features.impermanence.files = [ ".git-credentials" ];

    # commit message stolen from https://gist.github.com/lisawolderiksen/a7b99d94c92c6671181611be1641c733
    xdg.configFile."git/commitTemplate.txt".source = ./commitTemplate.txt;
    home.packages = [ pkgs.git-sync ];
    programs = {
      git = {
        enable = true;
        userName = "mayrf";
        userEmail = "70516376+mayrf@users.noreply.github.com";
        extraConfig = {
          commit = {
            template = "${config.xdg.configHome}/git/commitTemplate.txt";
          };
          credential = { helper = "store"; };
        };

      };
    };

  };
}
