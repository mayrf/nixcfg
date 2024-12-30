{ inputs }: {
  username = "mayrf";
  userEmail = inputs.nix-secrets.user-email;
  workEmail = inputs.nix-secrets.work-email;
  flakeDir = "/etc/nixos";
  persistDir = "/persist/system";
  persistDirNoBak = "/persist/no_bak";
  persistDirRoot = "/persist";
  # isMinimal = false; # Used to indicate nixos-installer build
}
