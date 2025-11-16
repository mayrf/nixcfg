{ lib
, buildNpmPackage
, fetchFromNpm
}:

buildNpmPackage rec {
  pname = "herb-language-server";
  version = "0.8.0";

  # Fetch from npm registry directly (simpler than GitHub monorepo)
  src = fetchFromNpm {
    pname = "@herb-tools/language-server";
    version = version;
    hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";  # Placeholder
  };

  npmDepsHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";  # Placeholder

  dontNpmBuild = false;

  meta = with lib; {
    description = "Language Server Protocol integration for HTML-aware ERB parsing using the Herb Parser";
    homepage = "https://herb-tools.dev/projects/language-server";
    changelog = "https://github.com/marcoroth/herb/releases/tag/v${version}";
    license = licenses.mit;
    maintainers = [ ];  # Add your GitHub username
    mainProgram = "herb-language-server";
  };
}
