{
  description = "Minimal NixOS configuration for bootstrapping systems";

  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    disko.url =
      "github:nix-community/disko"; # Declarative partitioning and formatting
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      inherit (self) outputs;

      configVars = import ./vars { inherit inputs; };
      minimalSpecialArgs = {
        inherit inputs outputs configVars;
        lib = nixpkgs.lib;
        # lib = nixpkgs.lib.extend
        #   (self: super: { custom = import ../lib { inherit (nixpkgs) lib; }; });
        #
        aidLib = import ../aidLib/default.nix {
          inherit (nixpkgs) lib inputs;
        };
      };
      # This mkHost is way better: https://github.com/linyinfeng/dotfiles/blob/8785bdb188504cfda3daae9c3f70a6935e35c4df/flake/hosts.nix#L358
      newConfig = name: device:
        (nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = minimalSpecialArgs // { inherit device; };
          modules = [
            inputs.disko.nixosModules.disko
            inputs.nixos-wsl.nixosModules.wsl
            ./minimal-configuration.nix
            ../hosts/${name}/hardware-configuration.nix
            (if device != null then import ../hosts/${name}/disko.nix { inherit device; } else {})
            (if name == "radium" then import ../hosts/${name}/proxy-vars.nix else {})
            { networking.hostName = name; }
          ];
        });
    in {
      nixosConfigurations = {
        # host = newConfig "name" disk" "withSwap" "swapSize"
        # Swap size is in GiB
        helium = newConfig "helium" "/dev/sda";
        radium = newConfig "radium" null;
        # guppy = newConfig "guppy" "/dev/vda" false "0";

        #TODO:(gusto) uncomment when gusto gets moved to disko, until then flake check errors on this because gustos current hw config doesn't match the disko spec that installer uses
        #gusto = newConfig "gusto" "/dev/sda" true "8";

      };
    };
}
