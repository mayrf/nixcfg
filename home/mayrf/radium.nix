{ inputs, pkgs, ... }:

{
  imports = [
    ../common
    ../features/general/impermanence.nix
    ../features/general/ensure-secrets-repo.nix
    ../features/general/ensure-private-config-repo.nix
    ../features/general/ensure-config-repo.nix
    ../features/cli/zsh.nix
    ../features/cli/fzf.nix
    ../features/cli/ai.nix
    ../features/cli/development.nix
    ../features/cli/k8s.nix
    ../features/cli/lf
    ../features/cli/git
    ../features/cli/scripts
    ../features/cli/sops.nix
    ../features/cli/yazi.nix
    ../features/cli/syncthing.nix
    ../features/editor/emacs
    ../features/editor/nvim.nix
    ../features/editor/zed.nix
    inputs.dotfiles-private.outputs.homeManagerModules
  ];

  home.packages = with pkgs; [
    mariadb
    grafana-alloy
    firefox
    camunda-modeler
  ];

  colorscheme = inputs.nix-colors.colorschemes.woodland;

  features.private.work.enable = true;
}
