{ pkgs, inputs, ... }: {

  imports = [
    ./emacs
    ./nvim.nix
    ./vscode.nix
    ./zed.nix
    inputs.dotemacs.homeConfigurations.x86_64-linux.dotemacs
  ];

  home.packages = with pkgs; [
  ];
}
