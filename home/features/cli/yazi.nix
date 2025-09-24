{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.features.cli.yazi;
  # yazi-plugins = pkgs.fetchFromGitHub {
  #   owner = "yazi-rs";
  #   repo = "plugins";
  #   rev = "...";
  #   # hash = "sha256-...";
  # };
in {
  options.features.cli.yazi.enable = mkEnableOption "my yazi config";
  config = mkIf cfg.enable {
    programs.yazi = {
      enable = true;
      enableZshIntegration = true;
      plugins = {
        bookmarks = pkgs.fetchFromGitHub {
          # https://github.com/dedukun/bookmarks.yazi
          owner = "dedukun";
          repo = "bookmarks.yazi";
          rev = "9ef1254d8afe88aba21cd56a186f4485dd532ab8";
          sha256 = "GQFBRB2aQqmmuKZ0BpcCAC4r0JFKqIANZNhUC98SlwY=";
        };
        smart-enter = pkgs.yaziPlugins.smart-enter;
        
      };
      # theme = { };
      # settings = { };
      keymap = {
        manager.prepend_keymap = [
          {
            on = "m";
            run = "plugin bookmarks save";
            desc = "Save current position as a bookmark";
          }
          {

            on = "'";
            run = "plugin bookmarks jump";
            desc = "Jump to a bookmark";
          }
          {

            on = [ "b" "d" ];
            run = "plugin bookmarks delete";
            desc = "Delete a bookmark";
          }
          {
            on = [ "b" "D" ];
            run = "plugin bookmarks delete_all";
            desc = "Delete all bookmarks";
          }

          {
            on = ["L" "z" "z" ];
            run = ''shell 'zip -r \"$${@%.*}.zip\" \"$@\"' --confirm'';
            desc = "Zip selected";
          }

          {
            on = ["L" "z" "u" ];
            run = ''shell 'unzip \"$0\"' --confirm'';
            desc = "Unzip";
          }
          # { on = [ "z", "u" ], run = "shell 'unzip \"$0\"' --confirm", desc = "Unzip" },
        ];
      };
    };
    #     initLua = ''
    #  require
    # '';
  };
}
