{ ... }:
{
  flake.modules.homeManager.vscode =
    { config, pkgs, ... }:
    {
      features.impermanence.directories = [
        ".vscode"
        ".config/Code"
      ];
      nixpkgs.config.allowUnfree = true;
      programs.vscode = {
        enable = true;
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
          ];
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
            "[nix]" = {
              "editor.defaultFormatter" = "brettm12345.nixfmt-vscode";
            };
            "remote.SSH.useLocalServer" = false;
          };
        };
      };
    };
}
