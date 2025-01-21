{ inputs }: {
  username = "mayrf";
  userEmail = inputs.nix-secrets.user-email;
  workEmail = inputs.nix-secrets.work-email;
  flakeDir = "/etc/nixos";
  persistDir = "/persist/system";
  persistDirNoBak = "/persist/no_bak";
  persistDirRoot = "/persist";
  # impermanence = if host == "yttrium" then true else false;
}
