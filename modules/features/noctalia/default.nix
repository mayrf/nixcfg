{ self, inputs, ... }:
{

  # package = self'.packages.myNoctalia;
  perSystem =
    { pkgs, ... }:
    {
      packages.myNoctalia = inputs.wrapper-modules.wrappers.noctalia-shell.wrap {
        inherit pkgs; 

        # nix run nixpkgs#noctalia-shell ipc call state all > $FLAKE/modules/features/noctalia/noctalia.json
        settings = (builtins.fromJSON (builtins.readFile ./noctalia.json)).settings;
      };
    };
}
