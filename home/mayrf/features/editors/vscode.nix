{ config, lib, pkgs, ... }:

{
  programs.vscode = {
    enable = true;
    # package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      vscodevim.vim
      ms-python.python
      esbenp.prettier-vscode
      matklad.rust-analyzer
      eamodio.gitlens

      # TODO Find fix
      # The following plugin need to be installed manually for now as they are
      # either not in the nix store or the need to write to a plugin dir, which
      # is not possible if the dir in the nix store

      # goessner.mdmath
      # rubymaniac.vscode-direnv
      # ms-toolsai.jupyter
    ];
    mutableExtensionsDir = true;
    userSettings = {
      "terminal.integrated.profiles.linux".zsh.path =
        "/run/current-system/sw/bin/zsh";
      "git.autofetch" = true;
      "security.workspace.trust.untrustedFiles" = "open";
      "vim.useSystemClipboard" = true;
      "extensions.autoCheckUpdates" = false;
      "extensions.autoUpdate" = false;
      "notebook.experimental.useMarkdownRenderer" = true;
      "editor.formatOnSave" = true;
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
      "typescript.preferences.importModuleSpecifier" = "relative";
      "typescript.updateImportsOnFileMove.enabled" = "always";
      "explorer.confirmDragAndDrop" = false;
      "[javascript]" = {
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
      };
    };
  };
}
