{
  pkgs,
  lib,
  username,
  homeDirectory,
  zen-browser,
  llm-agents,
  ...
}:
{
  imports = [
    ./apps/fish
    ./apps/gitconfig
    ./apps/ssh
    ./apps/emacs
    ./apps/bash
    ./apps/kitty
    ./apps/neovim
    ./apps/kakoune
    # Niri desktop environment modules
    ./apps/niri
    ./apps/waybar
    ./apps/fuzzel
    ./apps/mako
    ./apps/swaylock
    ./apps/swayidle
    ./apps/swaybg
    ./apps/darkman
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

  home.packages = with pkgs; [
    aider-chat-with-playwright
    ansible
    automake
    awscli2
    brave
    bat
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
    llm-agents.packages.${pkgs.system}.opencode
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
    nixfmt-rfc-style
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
    elan
    racket
    discord
    plexamp
    xivlauncher
    exercism
    fwupd
    gcc
    zen-browser.packages."${system}".default

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
