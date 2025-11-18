# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal Nix configuration repository managing system configurations across multiple machines using Nix Flakes, nix-darwin (macOS), NixOS (Linux), and Home Manager. The repository uses a unified flake-based approach to declaratively manage both system-level and user-level configurations.

## System Architecture

### Configuration Structure

The repository is organized into three main layers:

1. **flake.nix**: Central entry point defining all system configurations and inputs
   - `darwinConfigurations`: macOS systems using nix-darwin
   - `nixosConfigurations`: Linux systems using NixOS

2. **systems/**: Per-machine system-level configurations
   - Each subdirectory (e.g., `proletariat`, `jormungand`, `cassiopeia`, `tachibana`, `laincomp`, `brain`) contains machine-specific settings
   - Contains `default.nix` or `configuration.nix` with system packages, services, and hardware configuration

3. **home/**: User-level configurations via Home Manager
   - `home-darwin.nix`: macOS home configuration (work machine)
   - `home-darwin-personal.nix`: macOS home configuration (personal machine)
   - `home-linux.nix`: Linux home configuration
   - `home/apps/`: Application-specific configurations as modular imports

### Key Systems

- **proletariat**: macOS work laptop (`aarch64-darwin`, user: `fagbabiaka`)
  - Uses AeroSpace window manager with custom keybindings
  - Has Sketchybar enabled for status bar
  - Includes work-specific packages (infra, dagger, GCP SDK)

- **jormungand**: macOS personal laptop (`aarch64-darwin`, user: `femi`)

- **cassiopeia**, **tachibana**, **laincomp**: NixOS Linux systems (`x86_64-linux`, user: `femi`)
  - `laincomp` uses Dell XPS 13 9380 hardware profile

- **brain**: NixOS WSL system (`x86_64-linux`, user: `nixos`)

### Flake Inputs

- `nixpkgs`: Custom fork with unfree packages enabled (`github:femiagbabiaka/nixpkgs-unfree`)
- `home-manager`: User environment management
- `nix-darwin`: macOS system management
- `mac-app-util`: macOS application linking
- `mkAlias`: Creates proper macOS aliases for Nix-installed apps
- `nixos-hardware`: Hardware-specific configurations
- `nixos-wsl`: WSL-specific NixOS configuration
- `dagger`: CI/CD tooling
- `neovim-nightly-overlay`: Latest Neovim builds

## Common Commands

### Building and Switching

```bash
# macOS: Build and activate system configuration
darwin-rebuild switch --flake .#<hostname>

# macOS example for current machine (proletariat)
darwin-rebuild switch --flake .#proletariat

# NixOS: Build and activate system configuration
sudo nixos-rebuild switch --flake .#<hostname>

# NixOS example
sudo nixos-rebuild switch --flake .#cassiopeia
```

### Development Workflow

```bash
# Update all flake inputs
nix flake update

# Update specific input
nix flake lock --update-input nixpkgs

# Check flake for errors
nix flake check

# Show flake outputs
nix flake show

# Build without switching (dry-run)
darwin-rebuild build --flake .#<hostname>
nixos-rebuild build --flake .#<hostname>
```

### Testing and Validation

```bash
# Format Nix files
nixfmt-rfc-style **/*.nix

# Check Nix syntax
nix-instantiate --parse <file>.nix

# Evaluate specific attribute
nix eval .#darwinConfigurations.proletariat.config.system.stateVersion
```

### Home Manager

Home Manager is integrated into the system configurations, not standalone. Changes to `home/` files require a full system rebuild:

```bash
# macOS
darwin-rebuild switch --flake .#<hostname>

# NixOS
sudo nixos-rebuild switch --flake .#<hostname>
```

## Application Configuration

Application configs are modularized in `home/apps/`. Each app directory typically contains:
- `default.nix`: Main configuration and package installation
- Additional config files (e.g., `configuration.org` for Emacs)

Currently configured applications:
- **Editors**: Emacs, Neovim, Helix, Kakoune, VSCode
- **Terminals**: Kitty, Alacritty
- **Shell**: Fish
- **Development**: Git, SSH, Tmux
- **Window Management** (Linux): i3

To add a new application configuration:
1. Create `home/apps/<appname>/default.nix`
2. Add import to appropriate home configuration file (`home-darwin.nix`, `home-linux.nix`, etc.)

## macOS-Specific Notes

### Application Linking

The `scripts/aliasApplications.nix` module replaces Home Manager's default app linking with proper macOS aliases using `mkAlias`. This ensures Nix-installed applications appear correctly in Spotlight and Launchpad.

### AeroSpace Window Manager

The `proletariat` machine uses AeroSpace (i3-inspired tiling window manager for macOS) with custom keybindings configured in `systems/proletariat/default.nix`. Key bindings follow i3 conventions with `alt` as the modifier key.

### Homebrew Integration

Some applications are still managed via Homebrew (defined in system configs):
- Casks: 1Password, Yubico Authenticator, Zen browser, Zed editor, etc.
- Mac App Store apps: Wireguard

## Package Management

### Custom Package Overrides

See `home-darwin.nix` for examples of package customization:
- `myInfra`: Version-pinned override of the `infra` package
- `myGCSDK`: Google Cloud SDK with additional components

### Unfree Packages

This configuration enables unfree packages via a custom nixpkgs fork and `nixpkgs.config.allowUnfree = true` in home configurations.

## Important Patterns

### Username and Home Directory

System configurations pass `username` to Home Manager via `extraSpecialArgs`:
- macOS work: `fagbabiaka`
- macOS personal: `femi`
- Linux: `femi` (except WSL which uses `nixos`)

Home Manager modules receive these as parameters and use them with `inherit username;`.

### State Version

All configurations use `stateVersion = "23.05"`. This should NOT be changed on existing systems.

## Formatting Standards

- Use `nixfmt-rfc-style` for formatting Nix code
- Follow the existing module structure when adding new configurations
- Keep machine-specific settings in `systems/` and user settings in `home/`
