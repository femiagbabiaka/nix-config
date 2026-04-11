{
  pkgs,
  lib,
  username,
  hostname,
  homeDirectory,
  zen-browser,
  llm-agents,
  ...
}:
{
  imports = [
    ./apps/bash
    ./apps/fish
    ./apps/gitconfig
    ./apps/hermes-agent
    ./apps/kakoune
    ./apps/kitty
    ./apps/neovim
    ./apps/pi
    ./apps/ssh
    ./apps/torrentbox
  ];

  home = {
    inherit username;
    homeDirectory = lib.mkDefault homeDirectory;
    stateVersion = "23.05";
  };

  programs.home-manager.enable = true;
  programs.fish.enable = true;
  # Note: nixpkgs.config is set at the system level when using useGlobalPkgs

  programs.firefox = {
    enable = true;
  };

  programs.pi = {
    enable = true;
    settings = {
      defaultProvider = "lemonade";
      defaultModel = "Qwen3.5-27B-GGUF";
      hideThinkingBlock = true;
      defaultThinkingLevel = "medium";
    };
    # Enable built-in extensions
    builtInExtensions = {
      # Safety extensions (recommended)
      permissionGate = true; # Confirm before rm -rf, sudo, etc.
      protectedPaths = true; # Block writes to .env, .git/, etc.

      # Productivity
      todo = true; # Todo list with /todos command

      # UI enhancements
      notify = true; # Desktop notifications
      summarize = true; # Summarize conversations
      statusLine = true; # Turn progress in footer
      titlebarSpinner = true; # Braille spinner while working

      # Useful features
      inlineBash = true; # Expand !{command} in prompts
      tools = true; # Interactive /tools command
      triggerCompact = true; # Auto-compact at 100k tokens
    };
  };

  home.packages = with pkgs; [
    ffmpeg
    uv
    gemini-cli-bin
    _1password-cli
    ansible
    automake
    awscli2
    brave
    bat
    bun
    cmake
    colima
    coreutils
    curl
    dockerfile-language-server
    fastly
    fd
    fq
    go
    golangci-lint
    golangci-lint-langserver
    google-cloud-sdk
    gh
    gotools
    gopls
    ghc
    ghostty
    tailscale
    ocaml
    zed-editor-fhs
    ocamlPackages.merlin

    chromium
    haskell-language-server
    helix
    mpv
    nodejs_25
    tailscale
    stack
    kubernetes-helm
    jujutsu
    jq
    k9s
    kubectx
    kakoune-lsp
    kak-tree-sitter
    mitschemeX11
    mosh
    nil
    nixd
    nixfmt
    nushell
    platinum-searcher
    pulseaudio
    rbenv
    rustup
    ripgrep
    shellcheck
    gnumake
    spotify
    stern
    toolbox
    _1password-gui
    yubioath-flutter
    terraform-docs
    tflint
    terraform-ls
    tfswitch
    tmux
    tmux-xpanes
    vscode-langservers-extracted
    yaml-language-server
    python315
    zig
    zls
    zstd
    zoxide
    zathura
    zellij
    fzf
    delta
    bat
    signal-desktop
    signal-cli
    elan
    racket
    discord
    plexamp
    xivlauncher
    exercism
    fwupd
    gcc
    zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.default

    # Wayland desktop utilities (for niri)
    brightnessctl
    grim
    slurp
    wl-clipboard
    cliphist
    pavucontrol
    blueman
    networkmanagerapplet
    kdePackages.dolphin
    swaylock-effects
    niri
  ];

  fonts.fontconfig.enable = true;
}
