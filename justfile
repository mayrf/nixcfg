SOPS_FILE := "~/code/sops/secrets/secrets.yaml"
SOPS_DIR := "~/code/sops"

# default recipe to display help information
default:
  @just --list

test:
  echo {{SOPS_FILE}}

update:
  nix flake update

diff:
  git diff ':!flake.lock'

# Add --option eval-cache false if you end up caching a failure you can't get around
rebuild:
  scripts/system-flake-rebuild.sh

rebuild-update: update && rebuild


update-nix-secrets:
  (cd {{SOPS_DIR}} && git fetch && git rebase) || true
  nix flake lock --update-input nix-secrets

commit-and-push-nix-secrets:
  (cd {{SOPS_DIR}} && git add {{SOPS_FILE}} && git commit -m "Update secrets" && git push)  
  nix flake lock --update-input nix-secrets

sops:
  (cd {{SOPS_DIR}} && git fetch && git rebase) || true
  echo "Editing {{SOPS_FILE}}"
  nix-shell -p sops --run "SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt sops {{SOPS_FILE}}"

rekey:
  cd {{SOPS_DIR}} && (\
    sops updatekeys -y secrets/secrets.yaml && \
    (pre-commit run --all-files || true) && \
    git add -u && (git commit -m "chore: rekey" || true) && git push \
  )
