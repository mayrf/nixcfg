{pkgs, ...}: {
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
    telescope.extensions = [
      {
        name = "fzf";
        packages = [pkgs.vimPlugins.telescope-fzf-native-nvim];
        setup = {fzf = {fuzzy = true;};};
      }
    ];
    autocomplete.nvim-cmp.enable = true;

    formatter.conform-nvim.enable = true;
    clipboard = {
      enable = true;
      registers = "unnamedplus";
    };

    languages = {
      enableTreesitter = true;
      enableLSP = true;
      nix = {
        extraDiagnostics.enable = true;
        lsp.enable = true;
        enable = true;
        treesitter.enable = true;
        format.enable = true;
      };
      ts.enable = true;
      ruby.enable = true;
    };
    utility.direnv.enable = true;
    binds = {
      cheatsheet.enable = true;
      whichKey.enable = true;
    };
  };
}
