{ inputs, ... }:

{
  imports =
    [ ../common ../features/terminal ../features/cli ../features/editor ];

  features = {
    cli = {
      zsh.enable = true;
      fzf.enable = true;
      ai.enable = true;
      development.enable = true;
      k8s.enable = true;
      lf.enable = true;
      git.enable = true;
      sops.enable = true;
      yazi.enable = true;
    };
    editor = {
      emacs.enable = true;
      nvim.enable = true;
    };
  };

  colorscheme = inputs.nix-colors.colorschemes.woodland;

}
