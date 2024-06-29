{ inputs, ... }:

{
  imports = [
    ./global
    ./linux
    ./features/terminal/kitty.nix
    ./features/editors/emacs
    ./features/editors/vscode.nix
  ];

  lf.enable = true;

  colorscheme = inputs.nix-colors.colorschemes.woodland;

}
