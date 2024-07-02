{ inputs }:
let
  aidLib = (import ./default.nix) { inherit inputs; };
  outputs = inputs.self.outputs;
in rec {

  moduleImports = [
    inputs.sops-nix.nixosModules.sops
    inputs.home-manager.nixosModules.home-manager
    inputs.nixos-wsl.nixosModules.wsl
    inputs.stylix.nixosModules.stylix
  ];

  pkgs-stable = import inputs.nixpkgs-stable {
    system = "x86_64-linux"; # System Architecture
    config.allowUnfree = true;
  };

  specialArgs = { inherit outputs inputs pkgs-stable; };

  mkSystem = config:
    inputs.nixpkgs.lib.nixosSystem {
      specialArgs = {
        user = config.user;
        host = config.host;
      } // specialArgs;
      modules = [
        config.nixosPath
        ../modules/nixos
        {
          home-manager.extraSpecialArgs = {
            user = config.user;
            host = config.host;
          } // specialArgs;
          home-manager.users.mayrf = {
            imports = [ config.homePath ../modules/home-manager ];
          };
        }
      ] ++ moduleImports;
    };

}
