{ lib, inputs }:
let
  aidLib = (import ./default.nix) { inherit inputs; };
  outputs = inputs.self.outputs;
  commonNixosModules = [
    inputs.sops-nix.nixosModules.sops
    inputs.home-manager.nixosModules.home-manager
    inputs.nixos-wsl.nixosModules.wsl
    inputs.stylix.nixosModules.stylix
    inputs.disko.nixosModules.default
    inputs.impermanence.nixosModules.impermanence
    inputs.dotfiles-private.outputs.nixosModules
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
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowUnfreePredicate = (_: true);
      permittedInsecurePackages = [ ];
    };
  };

  specialArgs = {
    inherit outputs inputs unstable stable;
    inherit (inputs.dotfiles-private) private;
  };
in rec {
  relativeToRoot = lib.path.append ../.;
  mkSystem = name:
    { nixosPath }:
    inputs.nixpkgs.lib.nixosSystem {
      specialArgs = specialArgs;
      modules = [
        nixosPath
      ] ++ commonNixosModules;
    };
}
