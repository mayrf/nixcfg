# ./modules/hosts.nix
{ inputs, ... }:
let
  outputs = inputs.self.outputs;  
  commonNixosModules = [
    inputs.sops-nix.nixosModules.sops
    inputs.home-manager.nixosModules.home-manager
    inputs.nixos-wsl.nixosModules.wsl
    inputs.stylix.nixosModules.stylix
    inputs.disko.nixosModules.default
    inputs.impermanence.nixosModules.impermanence
    inputs.nixos-cli.nixosModules.nixos-cli
  ];
  specialArgs = {
    inherit outputs inputs;
    inherit (inputs.dotfiles-private) private;
  };
in
{
  flake.nixosConfigurations.radium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules = with inputs.self.modules.nixos; [ ../hosts/radium ] ++ commonNixosModules;
  };
  flake.nixosConfigurations.yttrium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules = with inputs.self.modules.nixos; [ ../hosts/yttrium ] ++ commonNixosModules;
  };
  flake.nixosConfigurations.helium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules = with inputs.self.modules.nixos; [ ../hosts/helium ] ++ commonNixosModules;
  };
}
