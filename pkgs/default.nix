# You can build these directly using 'nix build .#example'

{ pkgs ? import <nixpkgs> { }, ... }: rec {
  # kcl-language-server = pkgs.callPackage ./kcl-language-server { };
  httpyac = pkgs.callPackage ./httpyac { };
  auger = pkgs.callPackage ./auger { };
  just-lsp = pkgs.callPackage ./just-lsp { };
  # kcl-lsp = pkgs.kclvm.overrideAttrs {
  #   postInstall = "";

  #   # meta = with lib; {
  #   #   description = "A lsp-server for the KCL programming language";
  #   #   homepage = "https://github.com/kcl-lang/lsp";
  #   #   license = licenses.asl20;
  #   #   platforms = platforms.linux ++ platforms.darwin;
  #   #   maintainers = with maintainers; [ selfuryon peefy ];
  #   # };
  #   buildPhase = ''
  #     echo "Building the application"
  #     cargo build --package kcl-language-server --release
  #   '';

  #   installPhase = ''
  #     mkdir -p $out/bin
  #     cp target/release/kcl-language-server $out/bin/
  #   '';
  # };
}
