{ buildGoModule, fetchFromGitHub, lib, pkgs }:

buildGoModule (finalAttrs: {
  pname = "argocd-lovely-plugin";
  version = "1.2.2";

  src = fetchFromGitHub {
    owner = "crumbhole";
    repo = finalAttrs.pname;
    tag = "${finalAttrs.version}";
    hash = "sha256-q5SeRG63YaWwM0uYhdt2gf/KQJ9qmczfEkoVpV35uFI=";
  };
  # + "/packages/tui";
  subPackages = [ "cmd/argocd-lovely-plugin" ];

  env.CGO_ENABLED = 0;

  # nativeBuildInputs = [ 
  #    pkgs.kustomize
  #    pkgs.helmfile
  #    pkgs.yq
  #    pkgs.helm
  #  ];
  # Disable testing
  doCheck = false;
  vendorHash = "sha256-NmD5dBA5vdKkI8+4cUEdDDYBefK91Hbk1RYGzWh4RTQ=";
  # ldflags = [ "-s" "-X=main.Version=${finalAttrs.version}" ];

  meta = with lib; {
    description = "A plugin to make Argo CD behave like we'd like.";
    homepage = "https://github.com/crumbhole/argocd-lovely-plugin";
    license = licenses.bsd3;
    maintainers = [ maintainers.ehllie ];
  };
})
