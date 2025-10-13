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
    ./apps/neovim
    ./apps/gitconfig
    ./apps/ssh
    ./apps/kakoune
  ];

  home = {
    inherit username;
    homeDirectory = lib.mkDefault homeDirectory;
    stateVersion = "23.05";
  };

  programs.home-manager.enable = true;
  programs.fish.enable = true;
  nixpkgs.config.allowUnfree = true;

  programs.firefox = {
    enable = true;
  };

  home.packages = with pkgs; [
    aider-chat-with-playwright
    ansible
    ansible-language-server
    automake
    awscli2
    bat
    cmake
    colima
    coreutils
    curl
    dockerfile-language-server-nodejs
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
    ripgrep
    roswell
    rustup
    shellcheck
    spotify
    stern
    tailscale
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
    nerd-fonts.fira-code
    fzf
    delta
    bat
  ];

  fonts.fontconfig.enable = true;
}
