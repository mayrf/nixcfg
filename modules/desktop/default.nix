{ inputs, self, ... }:
{
  flake.modules.homeManager.desktop =
    { config, ... }:
    {
      imports = [
        self.modules.homeManager.firefox
      ];
     
    };

  flake.modules.nixos.desktop =
    { config, ... }:
    {
      home-manager.users.${config.host.username} = {
        imports = [ self.modules.homeManager.desktop ];
      };
    };
}
