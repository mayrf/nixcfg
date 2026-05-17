{ self, inputs, ... }:
{

  # package = self'.packages.myNoctalia;
  perSystem =
    { pkgs, ... }:
    {
      packages.myNoctalia = inputs.wrapper-modules.wrappers.noctalia-shell.wrap {
        inherit pkgs; # THIS PART IS VERY IMPORTAINT, I FORGOT IT IN THE VIDEO!!!
        settings = (builtins.fromJSON (builtins.readFile ./noctalia.json)).settings;
      };
    };
}
