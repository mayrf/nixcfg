{ inputs, ... }:

{
  imports = [ ./global ./linux ./features/terminal/kitty.nix ];

  lf.enable = true;
  vscode.enable = true;
  emacs.enable = true;

  colorscheme = inputs.nix-colors.colorschemes.woodland;

}
