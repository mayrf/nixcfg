{ config, pkgs, ... }:
{
  features.impermanence.directories = [ ".config/git" ];
  features.impermanence.files = [ ".git-credentials" ];

  xdg.configFile."git/commitTemplate.txt".source = ./commitTemplate.txt;
  home.packages = [ pkgs.git-sync ];
  programs = {
    git = {
      enable = true;
      settings = {
        user = {
          email = "70516376+mayrf@users.noreply.github.com";
          name = "mayrf";
        };
        commit.template = "${config.xdg.configHome}/git/commitTemplate.txt";
        push.autoSetupRemote = true;
        credential.helper = "store";
      };
    };
  };
}
