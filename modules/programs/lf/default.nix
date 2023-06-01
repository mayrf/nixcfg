{ config, pkgs, ... }:

{
  xdg.configFile = {
    "lf/lfrc".source = ./lfrc;
    "lf/icons".source = ./icons;
    "lf/colors".source = ./colors;
  };
  programs.lf = {
    enable = true;
    # settings = {
    #   color256 = true;
    #   icons = true;
    #   drawbox = true;
    #   hidden = true;
    #   ignorecase = true;
    #   number = true;
    #   preview = true;
    #   ratios = "2:4:3";
    #   relativenumber = true;
    #   scrolloff = 10;
    #   dirfirst = true;
    #   # sortby = "";
    # };
    # extraConfig = ''
    #   set previewer ctpv;
    #   set cleaner ctpvclear;
    #   &ctpv -s $id
    #   &ctpvquit $id
    # '';
    # keybindings = {
    #   m = "";
    #   D = "trash";
    #   md = "mkdir";
    # };
    # commands = {
    # trash = ''
    #   IFS="$(printf '\n\t')"; trash $fx
    #   # echo test this $HOME:  $fx
    #   # printf "file exists $HOME"
    #   # IFS="$(printf '\n\t')"; mv $fx ~/.trash
    # '';
    # mkdir = ''
    #   ${{
    #           printf "Directory Name: "
    #           read ans
    #           mkdir $ans
    #         }}'';
    # };
  };
}
