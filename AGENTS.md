# Repository Guidelines

## Project Structure & Module Organization
This flake-driven repo defines both macOS (Darwin) and NixOS hosts. `flake.nix` is the single source of truth for inputs and host outputs; avoid duplicating state elsewhere. Host-specific modules live under `systems/<hostname>/`, with `default.nix` holding base options and `configuration.nix` for larger NixOS profiles. User environments are under `home/`, with top-level `home-darwin*.nix` and `home-linux.nix` that import per-application modules from `home/apps/<tool>/`. Keep supporting scripts and helpers in `scripts/` (currently housing `aliasApplications.nix` for macOS app aliases). New modules should follow this layout so hosts can opt in via `imports`.

## Build, Test, and Development Commands
- `nix flake check` — evaluates all system and home modules; run before pushing to catch syntax or option regressions.
- `nix build .#darwinConfigurations.proletariat.system` — dry-build the proletariat configuration; pair with `darwin-rebuild switch --flake .#proletariat` to apply on macOS.
- `nix build .#nixosConfigurations.cerebro.config.system.build.toplevel` — ensure the cerebro NixOS profile builds; apply with `sudo nixos-rebuild switch --flake .#cerebro`.
- `nix flake update` — refresh input pins; commit both `flake.nix` and `flake.lock`.

## Coding Style & Naming Conventions
Write Nix expressions with two-space indentation and align attribute assignments for readability. Prefer descriptive attribute names (`programs.fish`, `services.tailscale`) and keep lists alphabetised when order does not matter. Use snake_case for variables inside `let` blocks. Format code with `nixfmt-rfc-style` (`nix fmt` inside the repo) before committing; avoid manual line wrapping that fights the formatter. When adding Lua, Fish, or other dotfiles under `home/apps`, follow the conventions already present in that tool's directory.

## Testing Guidelines
Treat successful evaluation as the primary test signal. Run `nix flake check` plus the relevant `nix build` target for any host you touched. On macOS, prefer `darwin-rebuild --dry-run --flake .#<host>` before switching; on NixOS use `sudo nixos-rebuild test --flake .#<host>` when available. Keep new modules guarded behind host-specific feature flags so unaffected machines still evaluate cleanly.

## Commit & Pull Request Guidelines
Recent history favors short, imperative subjects (`swap to iosevka`, `add cerebro`). Use optional prefixes like `fix:` when they clarify intent. Group related changes per commit so rollbacks stay easy. For PRs, include: scope summary, affected hosts or profiles, any manual steps (e.g., secrets to provision), and screenshots only when they help verify GUI-facing tweaks. Link related issues or tickets; note required rebuild commands so reviewers can reproduce the change.

## Security & Configuration Tips
Never commit secrets; store tokens in your password manager or host keychain. SSH material under `home/apps/ssh` should reference files in `$HOME/.ssh` created out-of-band. When adding proprietary packages, gate them behind `nixpkgs.config.allowUnfree = true` and document why they are needed in the module header.
