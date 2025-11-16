{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nodejs,
}:

buildNpmPackage rec {
  pname = "herb-language-server";
  version = "0.8.0";

  src = fetchFromGitHub {
    owner = "marcoroth";
    repo = "herb";
    rev = "v${version}";
    hash = "sha256-gMyoBhe3Kq1Mk9zPUdyO/bwdPCQ/TDcPKAg7QkmeIJU="; # Run nix-prefetch-github marcoroth herb --rev v0.8.0
  };

  # Generate package-lock.json during the build
  prePatch = ''
    ${nodejs}/bin/npm install --package-lock-only
  '';

  # npmDepsHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="; # You'll get this from the build error

  # Specify which workspace to build
  npmWorkspace = "packages/language-server";

  # Don't run npm build as Nx handles this differently
  dontNpmBuild = true;

  # Build using Nx
  buildPhase = ''
    runHook preBuild
    
    npx nx build language-server
    
    runHook postBuild
  '';

  # Install the built package
  installPhase = ''
    runHook preInstall
    
    mkdir -p $out/bin
    mkdir -p $out/lib/node_modules/@herb-tools/language-server
    
    cp -r dist/packages/language-server/* $out/lib/node_modules/@herb-tools/language-server/
    cp -r node_modules $out/lib/node_modules/@herb-tools/language-server/
    
    # Create the binary wrapper
    cat > $out/bin/herb-language-server <<EOF
    #!/usr/bin/env bash
    exec ${nodejs}/bin/node $out/lib/node_modules/@herb-tools/language-server/main.js "\$@"
    EOF
    
    chmod +x $out/bin/herb-language-server
    
    runHook postInstall
  '';

  meta = {
    description = "Language Server Protocol integration for HTML-aware ERB parsing using the Herb Parser";
    homepage = "https://github.com/marcoroth/herb";
    license = lib.licenses.mit;
    mainProgram = "herb-language-server";
    maintainers = with lib.maintainers; [ ]; # Add your name
    platforms = lib.platforms.all;
  };
}
