{ inputs, ... }:
{
  flake.modules.nixos.claude =
    { pkgs, ... }:
    {
      nixpkgs.overlays = [ inputs.claude-code.overlays.default ];
      environment.systemPackages = [ pkgs.claude-code ];
    };
}
