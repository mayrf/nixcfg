{ pkgs, ... }:

{
  home.packages = with pkgs; [
    postman
    # needed for postman
    openssl
  ];
}
