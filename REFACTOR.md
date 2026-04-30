# NixOS Config Dendritic Refactor

Pattern reference: https://github.com/Doc-Steve/dendritic-design-with-flake-parts/wiki/Dendritic_Aspects

Template to follow: `modules/emacs/default.nix`

## Rules

- No `enable` flags — hosts import only the aspects they need
- NixOS + HM config for a feature live in the **same file** (`modules/<feature>/default.nix`)
- `flake.modules.nixos.<name>` wires HM via `home-manager.sharedModules`
- `flake.modules.homeManager.<name>` for the HM half
- Only the `nixos` module is imported in host configs (never `homeManager` directly)
- Host definitions: `modules/hosts/<hostname>/default.nix`
- Run `nix path-info --derivation .#nixosConfigurations.<host>.config.system.build.toplevel`
  for radium and yttrium after each step to verify no regressions
  (helium has a pre-existing `features.impermanence` eval error — not our fault)

## Status

### Done ✅
- `modules/emacs/default.nix` — dendritic, HM wired via `sharedModules`
- `modules/legacy/common-modules.nix` — defines `flake.modules.nixos.commonModules`
- `modules/hosts/radium/default.nix` — defines `flake.modules.nixos.radium` + `flake.nixosConfigurations.radium`
- `modules/hosts/yttrium/default.nix` — defines `flake.modules.nixos.yttrium` + `flake.nixosConfigurations.yttrium`
- `modules/hosts/helium/default.nix` — defines `flake.modules.nixos.helium` + `flake.nixosConfigurations.helium`
- `modules/hosts.nix` deleted (all 3 hosts migrated out)
- `modules/options/default.nix` — `flake.modules.nixos.base` (merged persistence + preferences options)
- `modules/impermanence/default.nix` — converted to `flake.modules.nixos.impermanence`
- `modules/claude.nix` — converted to `flake.modules.nixos.claude`
- `modules/sops/default.nix` — `flake.modules.nixos.sops`
- `modules/bluetooth/default.nix` — `flake.modules.nixos.bluetooth`
- `modules/docker/default.nix` — `flake.modules.nixos.docker`
- `modules/flatpak/default.nix` — `flake.modules.nixos.flatpak`
- `modules/gaming/default.nix` — `flake.modules.nixos.gaming`
- `modules/kanata/default.nix` — `flake.modules.nixos.kanata`
- `modules/laptop/default.nix` — `flake.modules.nixos.laptop`
- `modules/open-webui/default.nix` — `flake.modules.nixos.open-webui`
- `modules/pipewire/default.nix` — `flake.modules.nixos.pipewire`
- `modules/printing/default.nix` — `flake.modules.nixos.printing`
- `modules/theming/default.nix` — `flake.modules.nixos.theming`
- `modules/virtualisation/default.nix` — `flake.modules.nixos.virtualisation`
- `modules/winapps/default.nix` — `flake.modules.nixos.winapps`
- VPN (wireguard) config inlined in `modules/hosts/helium/default.nix`

- `modules/common/default.nix` — `flake.modules.nixos.common` (merged hosts/common + hosts/common/users)
- `hosts/common/` deleted
- `hosts/features/` deleted
- `flake.modules.homeManager.{yttrium,helium,radium}` — per-host HM modules (wrapping existing home/mayrf/*.nix)
- `modules/common/keys/` — SSH public keys (moved from hosts/common/users/keys/)
- `hosts/common` imports removed from all legacy host configs
- `hosts/features` imports removed from all legacy host configs
- `home/mayrf/${hostName}.nix` dynamic import removed from common; wired per-host instead

Current `/etc` baselines (after Steps E+F + cleanup):
- yttrium: `74vpy95pqb0w9sv2i15j58vm2szxr210-etc.drv`
- radium: `c84273aim704065wwrm3gv6a6k2p4lcd-etc.drv`
- helium: `jz0ddz6ll36id0yis2vhjwmw6i1zysrs-etc.drv`

### TODO

#### Step A — ✅ Fix helium's pre-existing breakage
Removed stale `features.impermanence.enable = true` from `hosts/helium/default.nix`.
Impermanence is already handled via `persistence.enable` in `modules/hosts/helium/default.nix`.

#### Step B — Migrate `modules/options/` + `modules/impermanence/` + `modules/claude.nix`

These still use the old `flake.nixosModules.*` pattern:
- `modules/options/persistance.nix` → `flake.nixosModules.base` (persistence options)
- `modules/options/user.nix` → `flake.nixosModules.base` (preferences/user options)
- `modules/impermanence/default.nix` → `flake.nixosModules.extra_impermanence`
- `modules/claude.nix` → `flake.nixosModules.claude`

These are imported by the host modules above as `self.nixosModules.*` — they work but
are not yet dendritic. Convert each to `flake.modules.nixos.*` and update all callers.

#### Step C — Migrate `hosts/features/*.nix` to `modules/<feature>/default.nix`

Drop `options.features.<name>.enable` pattern. Hosts will directly import the feature.

Current features (all in `hosts/features/`):
- bluetooth.nix
- desktop/ (default.nix + sub-features)
- devbox.nix
- docker.nix
- flatpak.nix
- gaming.nix
- kanata.nix
- keyd.nix
- laptop.nix
- nix-bitcoin.nix
- open-webui.nix
- pipewire.nix
- podman.nix
- printing.nix
- sddm.nix
- sops.nix
- theming.nix
- virtualisation.nix
- vpn.nix
- winapps.nix

Do one feature at a time. Update the host `modules/hosts/<hostname>/default.nix` to
import `self.modules.nixos.<feature>` instead of the old `features.<name>.enable = true`.
Verify drvs after each.

#### Step D — Migrate `home/features/*.nix` to `modules/<feature>/default.nix`

Each HM feature module becomes `flake.modules.homeManager.<name>` inside the same file
as its NixOS counterpart (Step C). If a feature is HM-only, it still lives in
`modules/<feature>/default.nix` as `flake.modules.homeManager.<name>`.

Current home features (`home/features/`):
- cli/ (zsh, fzf, ai, media, development, k8s, leetcode, yazi, scripts, lf, git, syncthing, sops)
- desktop/ (wayland, waybar, hyprland, gammastep, mako, wofi, nextcloud-client, virtualisation, postman, librewolf, gpg, zathura, learning, media, social, productivity, zen-browser, email)
- editor/ (nvim, emacs, vscode, zed)
- terminal/ (alacritty, foot, ghostty)

#### Step E — Migrate `hosts/common/` to `modules/common/default.nix`

`hosts/common/default.nix` and friends (users, nixos-cli) become a
`flake.modules.nixos.common` dendritic module. HM wiring for the common home-manager
config (`home/common/`) moves here via `home-manager.sharedModules`.

#### Step F — Migrate per-host HM entry points

`home/mayrf/{yttrium,helium,radium}.nix` currently imported by
`hosts/common/users/mayrf.nix` via dynamic path. After Step E+D these become
`flake.modules.homeManager.{yttrium,helium,radium}` and are wired in the host's
`flake.modules.nixos.<hostname>` via `home-manager.sharedModules`.

#### Step G — Delete legacy directories

Once all the above is done:
- `hosts/` — fully absorbed into `modules/`
- `home/` — fully absorbed into `modules/`
