{ lib, inputs, configVars }:
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

  unstable = import inputs.unstable {
    system = "x86_64-linux"; # System Architecture
    # config.allowUnfree = true;
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowUnfreePredicate = (_: true);
      permittedInsecurePackages = [ ];

    };
  };

  stable = import inputs.stable {
    system = "x86_64-linux"; # System Architecture
    # config.allowUnfree = true;
    #
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowUnfreePredicate = (_: true);
      permittedInsecurePackages = [ ];

    };
  };

  specialArgs = { inherit outputs inputs unstable stable configVars; };
in rec {
  relativeToRoot = lib.path.append ../.;
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
          # home-manager.backupFileExtension = "backup";
          home-manager.users.mayrf = {
            imports = [
              config.homePath
              inputs.impermanence.nixosModules.home-manager.impermanence
              inputs.nixvim.homeManagerModules.nixvim
            ] ++ (builtins.attrValues outputs.homeManagerModules);
          };
        }
      ] ++ moduleImports;
    };
}
