{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = github:nix-community/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager }: 
    let
      user = "mayrf";
      system = "x86-64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        config.pulseaudio.enable = true;
      };
      lib = nixpkgs.lib;
    in {
         nixosConfigurations = {
	   vm = lib.nixosSystem {
             inherit system;
             modules = [ 
               ./hosts/vm  
               home-manager.nixosModules.home-manager {
                 home-manager.useGlobalPkgs = true;
                 home-manager.useUserPackages = true;
                 home-manager.users.${user} = {
                   imports = [ ./home.nix ];
                 }; 
               }
             ];
           };
	   x220 = lib.nixosSystem {
             inherit system;
             modules = [ 
               ./hosts/x220 
               home-manager.nixosModules.home-manager {
                 home-manager.useGlobalPkgs = true;
                 home-manager.useUserPackages = true;
                 home-manager.users.${user} = {
                   imports = [ ./home.nix ];
                 }; 
               }
             ];
           };
         };
    };
}
