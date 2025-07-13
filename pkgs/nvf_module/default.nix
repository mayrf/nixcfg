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
    autocomplete.nvim-cmp.enable = true;

    formatter.conform-nvim.enable = true;

    languages = {
      enableLSP = true;
      enableTreesitter = true;

      nix = {
        enable = true;
        format.enable = true;
      };
      ts.enable = true;
      ruby.enable = true;
    };
  };
}
