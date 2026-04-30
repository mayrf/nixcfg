{ inputs, ... }:
{
  flake.modules.homeManager.nvim =
    { config, pkgs, lib, outputs, inputs, ... }:
    {
      imports = [ inputs.nixvim.homeModules.nixvim ];
      home.packages = with pkgs;
        [
          outputs.packages.${pkgs.stdenv.system}.my-neovim
        ];
    };
}
