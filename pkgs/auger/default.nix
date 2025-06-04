{ buildGoModule, fetchFromGitHub, lib,}:

buildGoModule (finalAttrs: {
  pname = "auger";
  version = "1.0.3";

  src = fetchFromGitHub {
    owner = "etcd-io";
    repo = finalAttrs.pname;
    tag = "v${finalAttrs.version}";
    hash = "sha256-xUUnu7fW9EmiAkNKDao9EkmZhVasepmK5uP1kSVFFIY";
  };

  vendorHash = "sha256-q3p6nhKVuMOvzxUM45QYGXo1pY78ET3ifEIyqPvy4PI=";

  meta = with lib; {
    description = "Directly access data objects stored in etcd by Kubernetes.";
    homepage = "https://github.com/etcd-io/auger";
    license = licenses.mit;
    maintainers = [ maintainers.ehllie ];
  };
})
