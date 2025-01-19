{ config, pkgs, stable, lib, ... }:
with lib;
let
  cfg = config.yazi;
  # yazi-plugins = pkgs.fetchFromGitHub {
  #   owner = "yazi-rs";
  #   repo = "plugins";
  #   rev = "...";
  #   # hash = "sha256-...";
  # };
in {
  options.yazi = { enable = mkEnableOption "my yazi config"; };
  config = mkIf cfg.enable {
    programs.yazi = {
      enable = true;
      enableZshIntegration = true;
      plugins = {
        bookmarks = pkgs.fetchFromGitHub {
          # https://github.com/dedukun/bookmarks.yazi
          owner = "dedukun";
          repo = "bookmarks.yazi";
          rev = "202e450b0088d3dde3c4a680f30cf9684acea1cc";
          sha256 = "sha256-cPvNEanJpcVd+9Xaenu8aDAVk62CqAWbOq/jApwfBVE";
        };
      };
      # theme = { };
      # settings = { };
      keymap = {
        manager.prepend_keymap = [
          {
            on = "m";
            run = "plugin bookmarks --args=save";
            desc = "Save current position as a bookmark";
          }
          {

            on = "'";
            run = "plugin bookmarks --args=jump";
            desc = "Jump to a bookmark";
          }
          {

            on = [ "b" "d" ];
            run = "plugin bookmarks --args=delete";
            desc = "Delete a bookmark";
          }
          {
            on = [ "b" "D" ];
            run = "plugin bookmarks --args=delete_all";
            desc = "Delete all bookmarks";
          }
        ];
      };
    };
    #     initLua = ''
    #  require
    # '';
  };
}
