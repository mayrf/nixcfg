
# default recipe to display help information
default:
  @just --list

update:
  nix flake update

diff:
  git diff ':!flake.lock'

# Add --option eval-cache false if you end up caching a failure you can't get around
rebuild:
  scripts/system-flake-rebuild.sh

rebuild-update: update && rebuild
