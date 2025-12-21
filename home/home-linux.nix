{
  pkgs,
  lib,
  username,
  homeDirectory,
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
    ./apps/kanata
  ];

  home = {
    inherit username;
    homeDirectory = lib.mkDefault homeDirectory;
    stateVersion = "23.05";
  };

  programs.home-manager.enable = true;
  programs.fish.enable = true;
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;

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
    haskell-language-server
    helix
    mpv
    stack
    kubernetes-helm
    jujutsu
    jq
    k9s
    kubectx
    kakoune-lsp
    mitschemeX11
    mosh
    nil
    nixfmt-rfc-style
    nushell
    platinum-searcher
    pulseaudio
    rbenv
    rustup
    ripgrep
    shellcheck
    spotify
    stern
    tailscale
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
  ];

  fonts.fontconfig.enable = true;
}
