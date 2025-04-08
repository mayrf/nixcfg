{ pkgs, ... }: {

  imports = [
    ./emacs
    ./nvim.nix
    ./vscode.nix
  ];

  home.packages = with pkgs; [
  ];
}
