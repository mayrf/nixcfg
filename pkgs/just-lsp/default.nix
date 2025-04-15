{  stdenv, rustPlatform, fetchFromGitHub  }:
rustPlatform.buildRustPackage rec {
  pname = "just-lsp";
  version = "0.2.0";

  src = fetchFromGitHub {
    owner = "terror";
    repo = pname;
    rev = version;
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
  };

  cargoLock = {
    lockFile = ./Cargo.lock;
    # outputHashes = {
    #   "inkwell-0.2.0" = "sha256-JxSlhShb3JPhsXK8nGFi2uGPp8XqZUSiqniLBrhr+sM=";
    #   "protoc-bin-vendored-3.1.0" =
    #     "sha256-RRqpPMJygpKGG5NYzD93iy4htpVqFhYMmfPgbRtpUqg=";
    # };
  };

  # cargoHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

  meta = with stdenv.lib; {
    description = "A language server for just ";
    homepage = "https://github.com/terror/just-lsp";
    # license = licenses.unlicense;
    # maintainers = [ maintainers.tailhook ];
  };
}
