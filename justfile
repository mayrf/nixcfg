SOPS_FILE := "~/.config/nix-secrets/secrets/secrets.yaml"
SOPS_DIR := "~/.config/nix-secrets"

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


sops SSH_KEY_PATH:
  (cd {{SOPS_DIR}} && git fetch && git rebase) || true
  echo "Editing {{SOPS_FILE}}"
  nix-shell -p sops --run "SOPS_AGE_KEY=$(nix-shell -p ssh-to-age --run 'cat {{SSH_KEY_PATH}} | ssh-to-age -private-key') ./scripts/sops_with_age_key.sh {{SOPS_FILE}} $(cat {{SSH_KEY_PATH}} | ssh-to-age -private-key)"


rekey:
  cd {{SOPS_DIR}} && (\
    sops updatekeys -y secrets/secrets.yaml && \
    (pre-commit run --all-files || true) && \
    git add -u && (git commit -m "chore: rekey" || true) && git push \
  )
