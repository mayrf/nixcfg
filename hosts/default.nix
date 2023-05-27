{ nixpkgs, home-manager, user, hyprland, ... }:
let
  system = "x86-64-linux";
  pkgs = import nixpkgs {
    inherit system;
    config.allowUnfree = true;
    config.pulseaudio.enable = true;
  };
  lib = nixpkgs.lib;
in {
  vm = lib.nixosSystem {
    inherit system;
    specialArgs = { inherit system user hyprland; };
    modules = [
      ./vm
      home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.${user} = { imports = [ ./../home.nix ]; };
      }
    ];
  };
  x220 = lib.nixosSystem {
    inherit system;
    specialArgs = { inherit system user hyprland; };
    modules = [
      ./x220
      home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.${user} = { imports = [ ./../home.nix ]; };
      }
    ];
  };
}
