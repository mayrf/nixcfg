{ lib, stdenv, rustPlatform, fetchFromGitHub, protobuf, pkg-config, darwin
, rustc, }:
rustPlatform.buildRustPackage rec {
  pname = "kcl-language-server";
  version = "0.11.2";

  src = fetchFromGitHub {
    owner = "kcl-lang";
    repo = "kcl";
    rev = "v${version}";
    hash = "sha256-6XDLxTpgENhP7F51kicAJB7BNMtX4cONKJApAhqgdno=";
  };
  sourceRoot = "source/kclvm";
  cargoHash = "sha256-eJ3Gh2l6T2DxJRQRHamPOr/ILtzsqFB497DdXVJ90RE=";

  buildInputs = [ rustc ] ++ lib.optionals stdenv.isDarwin [
    darwin.apple_sdk.frameworks.Security
    darwin.apple_sdk.frameworks.CoreServices
    darwin.apple_sdk.frameworks.SystemConfiguration
  ];

  postInstall = lib.optionalString stdenv.isDarwin ''
    install_name_tool -id $out/lib/libkclvm_cli_cdylib.dylib $out/lib/libkclvm_cli_cdylib.dylib
  '';

  nativeBuildInputs = [ pkg-config protobuf ];

  patches = [ ./enable_protoc_env.patch ];

  PROTOC = "${protobuf}/bin/protoc";
  PROTOC_INCLUDE = "${protobuf}/include";

  buildPhase = ''
    echo "Building the application"
    cargo build --package kcl-language-server --release
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp target/release/kcl-language-server $out/bin/
  '';

  meta = with lib; {
    description = "A lsp-server for the KCL programming language";
    homepage = "https://github.com/kcl-lang/lsp";
    license = licenses.asl20;
    platforms = platforms.linux ++ platforms.darwin;
    maintainers = with maintainers; [ selfuryon peefy ];
  };

}
