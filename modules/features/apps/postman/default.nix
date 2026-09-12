{ ... }:
{
  flake.modules.homeManager.postman =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        postman
        openssl
      ];
    };
}
