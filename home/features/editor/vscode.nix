{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.editor.vscode;
in {
  options.features.editor.vscode.enable = mkEnableOption "my vscode user config";
  config = mkIf cfg.enable {
    nixpkgs.config.allowUnfree = true;
    programs.vscode = {
      enable = true;
      # package = pkgs.vscodium;
      package = pkgs.vscode.fhs;
      mutableExtensionsDir = true;
      profiles.default = {

        extensions = with pkgs.vscode-extensions; [
          vscodevim.vim
          ms-python.python
          esbenp.prettier-vscode
          eamodio.gitlens
          bbenoist.nix
          brettm12345.nixfmt-vscode

          # TODO Find fix
          # The following plugin need to be installed manually for now as they are
          # either not in the nix store or the need to write to a plugin dir, which
          # is not possible if the dir in the nix store

          # goessner.mdmath
          # rubymaniac.vscode-direnv
          # ms-toolsai.jupyter
        ];
        userSettings = {
          # lib.mkIf (config.networking.system == "aarch64-darwin")

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
          "[nix]" = {
            "editor.defaultFormatter" = "brettm12345.nixfmt-vscode";
          };
          "remote.SSH.useLocalServer" = false;
        };
      };
    };
  };
}
