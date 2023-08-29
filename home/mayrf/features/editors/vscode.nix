{ config, lib, pkgs, ... }:

{
  programs.vscode = {
    enable = true;
    # package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      vscodevim.vim
      ms-python.python
      # goessner.mdmath
      esbenp.prettier-vscode
      matklad.rust-analyzer
      eamodio.gitlens
    ];
    mutableExtensionsDir = true;
    userSettings = {
      "terminal.integrated.profiles.linux".zsh.path = "/run/current-system/sw/bin/zsh";
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
