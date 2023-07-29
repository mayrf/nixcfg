{ outputs, inputs }:
let
  addPatches = pkg: patches: pkg.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or [ ]) ++ patches;
  });
in
{
  # flake-inputs = final: _: {
  #   inputs = builtins.mapAttrs
  #     (_: flake: (flake.legacyPackages or flake.packages or { }).${final.system} or { })
  #     inputs;
  # };
}
