# ./modules/hosts.nix
{ inputs, self, ... }:
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

  flake.nixosModules.radium =
    { pkgs, config, ... }:
    {
      imports = [
        self.nixosModules.base
        self.nixosModules.extra_impermanence
        ../hosts/radium
      ];
    };

  flake.nixosConfigurations.radium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules =
      # with inputs.self.modules.nixos;
      [
        self.nixosModules.radium
      ]
      ++ commonNixosModules;
  };

  flake.nixosModules.yttrium =
    { pkgs, config, ... }:
    {
      imports = [
        self.nixosModules.base
        self.nixosModules.extra_impermanence
        ../hosts/yttrium
      ];
      persistence.enable = true;
      persistence.user = config.preferences.user.name;
    };

  flake.nixosConfigurations.yttrium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules =
      # with inputs.self.modules.nixos;
      [
        self.nixosModules.yttrium
      ]
      ++ commonNixosModules;
  };

  flake.nixosModules.helium =
    { pkgs, config, ... }:
    {
      imports = [
        self.nixosModules.base
        self.nixosModules.extra_impermanence
        ../hosts/helium
      ];
      persistence.enable = true;
      persistence.user = config.preferences.user.name;
    };

  flake.nixosConfigurations.helium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules =
      # with inputs.self.modules.nixos;
      [
        self.nixosModules.helium
      ]
      ++ commonNixosModules;
  };
}
