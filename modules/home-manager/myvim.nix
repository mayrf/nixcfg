{ config, pkgs, lib, inputs, ... }:
with lib;
let cfg = config.myvim;
in {
  options.myvim = { enable = mkEnableOption "my myvim user config"; };
  config = mkIf cfg.enable {
    programs.nixvim = {
      enable = true;
      # clipboard.register = "unnamedplus";
      opts = {
        # Show line numbers
        number = true;
        # You can also add relative line numbers, to help with jumping.
        #  Experiment for yourself to see if you like it!
        #relativenumber = true

        # Enable mouse mode, can be useful for resizing splits for example!
        mouse = "a";

        # Don't show the mode, since it's already in the statusline
        showmode = false;

        #  See `:help 'clipboard'`
        clipboard = {
          providers = {
            wl-copy.enable = true; # For Wayland
          };

          # Sync clipboard between OS and Neovim
          #  Remove this option if you want your OS clipboard to remain independent.
          register = "unnamedplus";
        };
      };
      defaultEditor = true;
      globals = {
        # Set <space> as the leader key
        # See `:help mapleader`
        mapleader = " ";
        maplocalleader = " ";

        # Set to true if you have a Nerd Font installed and selected in the terminal
        # have_nerd_font = false;
      };
      colorschemes.catppuccin.enable = true;
      plugins.lualine.enable = true;
    };

    # programs = {
    #   neovim = {
    #     enable = true;
    #     defaultEditor = true;
    #     viAlias = true;
    #     vimAlias = true;
    #     extraConfig = ''
    #       set clipboard+=unnamedplus
    #       set number relativenumber
    #       set shiftwidth=4
    #       set shiftwidth=4
    #     '';
    #   };
    # };
  };

}
