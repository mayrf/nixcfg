{
  programs = {
    neovim = {
      enable = true;
      defaultEditor = true;
      viAlias = true;
      vimAlias = true;
      extraConfig = ''
        set clipboard+=unnamedplus
        set number relativenumber
      '';
    };
  };
}
