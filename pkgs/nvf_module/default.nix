{ pkgs, ... }: {
  config.vim = {
    theme = {
      enable = true;
      # name = "gruvbox";
      name = "rose-pine";
      # style = "dark";
      style = "moon";
    };

    statusline.lualine.enable = true;
    telescope.enable = true;
    terminal.toggleterm.enable = true;
    telescope.extensions = [{
      name = "fzf";
      packages = [ pkgs.vimPlugins.telescope-fzf-native-nvim ];
      setup = { fzf = { fuzzy = true; }; };
    }];
    autocomplete.nvim-cmp.enable = true;
    projects.project-nvim.enable = true;
    projects.project-nvim.setupOpts.manual_mode = false;

    formatter.conform-nvim.enable = true;
    clipboard = {
      enable = true;
      registers = "unnamedplus";
    };

    lsp.formatOnSave = true;
    lsp.enable = true;
    languages = {
      enableTreesitter = true;
      enableFormat = true;
      enableExtraDiagnostics = true;
      enableDAP = true;
      yaml = { enable = true; };
      nix = { enable = true; };
      python.enable = true;
      ts = {
        enable = true;
        format = {
          enable = true;
          type = "prettier";
        };
      };
      ruby.enable = true;
    };
    utility.direnv.enable = true;
    binds = {
      cheatsheet.enable = true;
      whichKey.enable = true;
    };
  };
}
