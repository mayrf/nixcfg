{
  fonts = import ./fonts.nix;
  # wallpaper = import ./wallpaper.nix;
  lf = import ./lf;
  emacs = import ./emacs;
  vscode = ./vscode.nix;
  git = ./git;
  myvim = ./myvim.nix;
  myProton = ./protonmail.nix;
  myGhostty = ./ghostty.nix;
  yazi = ./yazi.nix;
  sops = ./sops.nix;
  email = ./email.nix;
  host-Spec = ../common/host-spec.nix;
}
