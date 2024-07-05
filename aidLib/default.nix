{ inputs }:
let
  aidLib = (import ./default.nix) { inherit inputs; };
  outputs = inputs.self.outputs;
  moduleImports = [
    inputs.sops-nix.nixosModules.sops
    inputs.home-manager.nixosModules.home-manager
    inputs.nixos-wsl.nixosModules.wsl
    inputs.stylix.nixosModules.stylix
    inputs.disko.nixosModules.default
    inputs.impermanence.nixosModules.impermanence
  ] ++ (builtins.attrValues outputs.nixosModules);

  pkgs-stable = import inputs.nixpkgs-stable {
    system = "x86_64-linux"; # System Architecture
    config.allowUnfree = true;
  };

  specialArgs = { inherit outputs inputs pkgs-stable; };
in rec {

  mkSystem = config:
    inputs.nixpkgs.lib.nixosSystem {
      specialArgs = {
        user = config.user;
        host = config.host;
      } // specialArgs;
      modules = [
        config.nixosPath
        {
          home-manager.extraSpecialArgs = {
            user = config.user;
            host = config.host;
          } // specialArgs;
          home-manager.users.mayrf = {
            imports = [
              config.homePath
              inputs.impermanence.nixosModules.home-manager.impermanence
            ] ++ (builtins.attrValues outputs.homeManagerModules);
          };
        }
      ] ++ moduleImports;
    };

}
