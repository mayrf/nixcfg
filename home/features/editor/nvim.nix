{ config, pkgs, lib, inputs, ... }:
with lib;
let cfg = config.features.editor.nvim;
in {
  imports = [ inputs.nixvim.homeManagerModules.nixvim ];
  options.features.editor.nvim = {
    enable = mkEnableOption "my myvim user config";
  };
  config = mkIf cfg.enable {
    home.packages = with pkgs; [ neovim nixd alejandra gnumake ];

    programs.nixvim = {
      #imports = [./keymaps.nix];
      enable = true;
      viAlias = true;
      vimAlias = true;
      clipboard.register = "unnamedplus";
      globalOpts = {
        # Tab defaults (might get overwritten by an LSP server)
        tabstop = 4;
        shiftwidth = 4;
        softtabstop = 0;
        expandtab = true;
        smarttab = true;

        # System clipboard support, needs xclip/wl-clipboard
        clipboard = {
          providers = {
            wl-copy.enable = true; # Wayland
            xsel.enable = true; # For X11
          };
          register = "unnamedplus";
        };

        # Save undo history
        undofile = true;
        # Start scrolling when the cursor is X lines away from the top/bottom
        scrolloff = 5;
        # Always show the signcolumn, otherwise text would be shifted when displaying error icons
        signcolumn = "yes";

        # Enable mouse
        mouse = "a";

        # Search
        ignorecase = true;
        smartcase = true;

        # Configure how new splits should be opened
        splitright = true;
        splitbelow = true;
      };
      keymaps = [
        # Neo-tree bindings
        {
          action = "<cmd>Neotree toggle<CR>";
          key = "<leader>e";
        }
        # Lazygit
        {
          mode = "n";
          key = "<leader>gg";
          action = "<cmd>LazyGit<CR>";
          options = { desc = "LazyGit (root dir)"; };
        }

        # Telescope bindings

        {
          action = "<cmd>Telescope live_grep<CR>";
          key = "<leader>fw";
        }
        {
          action = "<cmd>Telescope find_files<CR>";
          key = "<leader>ff";
        }
        {
          action = "<cmd>Telescope git_commits<CR>";
          key = "<leader>fg";
        }
        {
          action = "<cmd>Telescope oldfiles<CR>";
          key = "<leader>fh";
        }
        {
          action = "<cmd>Telescope colorscheme<CR>";
          key = "<leader>ch";
        }
        {
          action = "<cmd>Telescope man_pages<CR>";
          key = "<leader>fm";
        }
      ];
      opts = {
        # Show line numbers
        number = true;
        relativenumber = true; # Show relative line numbers
        # You can also add relative line numbers, to help with jumping.
        #  Experiment for yourself to see if you like it!
        #relativenumber = true

        # Enable mouse mode, can be useful for resizing splits for example!
        mouse = "a";

        # Don't show the mode, since it's already in the statusline
        showmode = false;
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
      # colorschemes.catppuccin.enable = true;
      plugins.lualine.enable = true;
      plugins = {
        oil = { enable = true; };
        treesitter = { enable = true; };
        telescope = {
          enable = true;
          extensions = { fzf-native = { enable = true; }; };
        };
        lsp.servers.nil_ls.enable = true;
        lsp.enable = true;
        lsp.servers.nixd.enable = true;
        lsp.servers.nixd.settings.formatting.command = [ "alejandra" ];
        lsp.servers.nixd.settings.nixpkgs = { expr = "import <nixpkgs> { }"; };
        web-devicons.enable = true;
      };
    };
  };
}
