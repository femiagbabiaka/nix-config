{
  pkgs,
  username,
  lib ? pkgs.lib,
  extraImports ? [ ],
  extraPackages ? [ ],
  homeStateVersion ? "23.05",
  homeExtraConfig ? { },
  configOverrides ? { },
  ...
}:
let
  inherit (lib) unique;

  sharedImports = [
    ../apps/bash
    ../apps/fish
    ../apps/gitconfig
    ../apps/kitty
    ../apps/neovim
    ../apps/emacs
    ../apps/kakoune
  ];

  basePackages = with pkgs; [
    ansible
    automake
    awscli2
    bat
    broot
    claude-code
    cmake
    codex
    coreutils
    crane
    curl
    delta
    delve
    dive
    docker
    docker-compose
    elan
    fd
    fish
    fishPlugins.done
    fishPlugins.hydro
    fontconfig
    fq
    fzf
    gh
    git
    git-crypt
    go
    golangci-lint
    golangci-lint-langserver
    gopls
    gotools
    graphviz
    haskell-language-server
    haskell.compiler.ghc910
    helix
    helm-ls
    htop
    jq
    jujutsu
    k9s
    nixd
    kakoune-lsp
    kak-tree-sitter-unwrapped
    kubernetes-helm
    lldb
    mosh
    neofetch
    nerd-fonts.fira-code
    nil
    nixfmt-rfc-style
    nodejs
    nushell
    platinum-searcher
    radicle-node
    radicle-tui
    rbenv
    ripgrep
    rustup
    shellcheck
    stern
    terraform-docs
    terraform-ls
    tflint
    tfswitch
    tilt
    vscode-langservers-extracted
    wget
    yaml-language-server
    zig
    zls
    zoxide
    zstd
    _1password-gui
    aider-chat-with-playwright
    ansible
    automake
    awscli2
    bat
    bat
    cmake
    colima
    coreutils
    curl
    delta
    dockerfile-language-server
    fastly
    fd
    fq
    fzf
    gh
    ghc
    go
    golangci-lint
    golangci-lint-langserver
    gopls
    gotools
    ghc
    haskell-language-server
    helix
    jq
    jujutsu
    k9s
    kakoune-lsp
    kubectx
    kubernetes-helm
    mosh
    mpv
    nerd-fonts.fira-code
    nil
    nixfmt-rfc-style
    nushell
    platinum-searcher
    pulseaudio
    rbenv
    ripgrep
    rustup
    shellcheck
    stack
    stern
    tailscale
    terraform-docs
    terraform-ls
    tflint
    tfswitch
    vscode-langservers-extracted
    yaml-language-server
    zathura
    zellij
    zig
    zls
    zoxide
    zstd
  ];
in
{
  imports = sharedImports ++ extraImports;

  home = {
    inherit username;
    stateVersion = homeStateVersion;
    packages = unique (basePackages ++ extraPackages);
  }
  // homeExtraConfig;

  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;

  fonts.fontconfig.enable = true;
}
// configOverrides
