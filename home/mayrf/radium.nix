{ inputs, ... }:

{
  imports = [
    ../common
    ../features/terminal
    ../features/cli
    # ../features/desktop
  ];

  features = {
    cli = {
      zsh.enable = true;
      fzf.enable = true;
    };
    terminal = {
      kitty.enable = true;
    };
  };
  lf.enable = true;
  # vscode.enable = true;
  emacs.enable = true;
  git.enable = true;

  colorscheme = inputs.nix-colors.colorschemes.woodland;

}
