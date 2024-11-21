{ buildNpmPackage, darwin, fetchFromGitHub, lib, python3, stdenv }:

buildNpmPackage rec {
  pname = "httpyac";
  version = "6.16.4";

  src = fetchFromGitHub {
    owner = "anweber";
    repo = pname;
    rev = version;
    hash = "sha256-qV6h5PCxOteQaToLHdpRYPGBFNFgr5WmIw3hTe3Edv0=";
    # hash = "sha256-DVLmB4WE+8p2i2l2aq7u/YefeEykKd3B7ekaq5vKUjI=";
  };

  npmDepsHash = "sha256-6szSBs4ltnxkSopuyPwV5KwEGXnlovVgSFtTzFMOHE4=";

  # env = { npm_config_build_from_source = true; };

  # nativeBuildInputs = [ python3 ];

  # buildInputs = lib.optionals stdenv.hostPlatform.isDarwin
  #   [ darwin.apple_sdk.frameworks.CoreServices ];

  meta = with lib; {
    description = "httpYac - Yet another Rest Client";
    homepage = "https://httpyac.github.io/";
    license = licenses.mit;
    # mainProgram = "nest";
    maintainers = [ maintainers.ehllie ];
  };
}
