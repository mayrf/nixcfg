{ lib, stdenv, rustPlatform, fetchFromGitHub, protobuf, pkg-config, darwin
, rustc, }:
rustPlatform.buildRustPackage rec {
  pname = "kcl-language-server";
  version = "0.9.3";

  src = fetchFromGitHub {
    owner = "kcl-lang";
    repo = "kcl";
    rev = "v${version}";
    # hash = "sha256-slU3n7YCV5VfvXArzlcITb9epdu/gyXlAWq9KLjGdJA=";
    hash = "sha256-nk5oJRTBRj0LE2URJqno8AoZ+/342C2tEt8d6k2MAc8=";
  };

  # sourceRoot = "source/kclvm/tools/src/LSP";
  sourceRoot = "source/kclvm";
  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "inkwell-0.2.0" = "sha256-JxSlhShb3JPhsXK8nGFi2uGPp8XqZUSiqniLBrhr+sM=";
      "protoc-bin-vendored-3.1.0" =
        "sha256-RRqpPMJygpKGG5NYzD93iy4htpVqFhYMmfPgbRtpUqg=";
    };
  };
  # cargoPatches = [ ./cargo.lock.patch ];

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

  meta = with lib; {
    description = "A lsp-server for the KCL programming language";
    homepage = "https://github.com/kcl-lang/lsp";
    license = licenses.asl20;
    platforms = platforms.linux ++ platforms.darwin;
    maintainers = with maintainers; [ selfuryon peefy ];
  };

  # Customize phases as needed
  # preBuild = ''
  #   echo "Entering the application's subdirectory"
  #   cd tools/src/LSP
  # '';

  # Define the build phase
  buildPhase = ''
    echo "Building the application"
    cargo build --package kcl-language-server --release
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp target/release/kcl-language-server $out/bin/
  '';

  # vendorHash = "sha256-Xv8Tfq9Kb1xGFCWZQwBFDX9xZW9j99td/DUb7jBtkpE=";

  # ldflags = [ "-w -s" "-X=kcl-lang.io/cli/pkg/version.version=v${version}" ];

  # subPackages = [ "cmd/kcl" ];

  # env vars https://github.com/kcl-lang/kcl-go/blob/main/pkg/env/env.go#L29
  # postFixup = ''
  #    wrapProgram $out/bin/kcl \
  #   --set PATH ${lib.makeBinPath [ kclvm_cli ]} \
  #   --set KCL_LIB_HOME ${lib.makeLibraryPath [ kclvm ]} \
  #   --set KCL_GO_DISABLE_INSTALL_ARTIFACT false \
  # '';

  # postInstall = ''
  #   installShellCompletion --cmd kcl \
  #     --bash <($out/bin/kcl completion bash) \
  #     --fish <($out/bin/kcl completion fish) \
  #     --zsh <($out/bin/kcl completion zsh)
  # '';

}
