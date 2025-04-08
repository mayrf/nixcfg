{pkgs, ...}: {
  imports = [
    ./alacritty.nix
    ./foot.nix
    ./kitty.nix
    ./ghostty.nix
  ];
}
