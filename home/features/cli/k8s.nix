{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.cli.k8s;
in {
  options.features.cli.k8s.enable = mkEnableOption "enable k8s cli programs";

  config = mkIf cfg.enable {

    features.impermanence.directories = [ ".kube" ];
    home.packages = with pkgs; [
      auger
      fluxcd
      kubectl
      kubectl-cnpg
      kubectl-tree 
      argocd
      kubernetes-helm
      crossplane-cli
      docker-credential-helpers # needed for kcl packages
      k9s
      argocd-lovely-plugin
      cue
      timoni
      stable.kcl-language-server
      stable.kcl
      kubernetes-helm
      argocd
      kargo
      stable.argocd-autopilot
      minikube
      kind
      kustomize
    ];
    home.sessionVariables = { KIND_EXPERIMENTAL_PROVIDER = "podman"; };
  };
}
