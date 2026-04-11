# ./modules/hosts.nix
{ inputs, self, ... }:
let
  outputs = inputs.self.outputs;
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
        self.modules.nixos.commonModules
        ../hosts/radium
      ];
    };

  flake.nixosConfigurations.radium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules = [
      self.nixosModules.radium
      self.modules.nixos.emacs
    ];
  };

  flake.nixosModules.yttrium =
    { pkgs, config, ... }:
    {
      imports = [
        self.nixosModules.base
        self.nixosModules.extra_impermanence
        self.modules.nixos.commonModules
        ../hosts/yttrium
        self.modules.nixos.emacs
      ];
      persistence.enable = true;
      persistence.user = config.preferences.user.name;
    };

  flake.nixosConfigurations.yttrium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules =
      [
        self.nixosModules.yttrium
      ];
  };

  flake.nixosModules.helium =
    { pkgs, config, ... }:
    {
      imports = [
        self.nixosModules.base
        self.nixosModules.extra_impermanence
        self.modules.nixos.commonModules
        ../hosts/helium
      ];
      persistence.enable = true;
      persistence.user = config.preferences.user.name;
    };

  flake.nixosConfigurations.helium = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = specialArgs;
    modules =
      [
        self.nixosModules.helium
      ];
  };
}
