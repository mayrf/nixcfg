{ pkgs, ... }: {
  imports = [
    ./general/ensure-secrets-repo.nix
    ./general/ensure-config-repo.nix
    ./general/ensure-private-config-repo.nix
    ./general/impermanence.nix
  ];
}
