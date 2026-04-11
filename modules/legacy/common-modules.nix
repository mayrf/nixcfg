{ inputs, ... }:
{
  flake.modules.nixos.commonModules =
    {
      ...
    }:
    {
      imports = [
        inputs.sops-nix.nixosModules.sops
        inputs.home-manager.nixosModules.home-manager
        inputs.nixos-wsl.nixosModules.wsl
        inputs.stylix.nixosModules.stylix
        inputs.disko.nixosModules.default
        inputs.impermanence.nixosModules.impermanence
        inputs.nixos-cli.nixosModules.nixos-cli
      ];
    };
}
