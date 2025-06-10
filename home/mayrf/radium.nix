{ inputs, pkgs, ... }:

{
  imports = [
    ../common
    ../features
    ../features/terminal
    ../features/cli
    ../features/editor
  ];

  features = {
    ensure-secrets-repo.enable = true;
    ensure-private-config-repo.enable = true;
    ensure-config-repo.enable = true;
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

  home.packages = with pkgs; [
      mariadb
      mysql-shell
      grafana-alloy
      firefox
  ];

  colorscheme = inputs.nix-colors.colorschemes.woodland;

}
