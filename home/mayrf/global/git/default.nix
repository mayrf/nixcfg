{ config, lib, pkgs, ... }:

{
  # commit message stolen from https://gist.github.com/lisawolderiksen/a7b99d94c92c6671181611be1641c733
  xdg.configFile."git/commitTemplate.txt".source = ./commitTemplate.txt;
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

}
