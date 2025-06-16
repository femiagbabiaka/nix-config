{ pkgs, username, ... }: {
  imports = [
    ./apps/vscode
    ./apps/fish
    ./apps/neovim
    ./apps/git
    ./apps/helix
    ./apps/kitty
    ./apps/alacritty
  ];

  home = {
    inherit username;
    stateVersion = "25.05";
  };

  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    aider-chat-with-playwright
    ansible
    ansible-language-server
    automake
    awscli2
    bat
    cmake
    lima
    colima
    coreutils
    crane
    curl
    docker
    docker-compose
    delve
    dive
    dockerfile-language-server-nodejs
    fastly
    fd
    fq
    gh
    go
    golangci-lint
    golangci-lint-langserver
    gopls
    gotools
    git-crypt
    graphviz
    helm-ls
    helix
    jq
    k9s
    kakoune
    kubectx
    kubernetes-helm
    lldb
    nerd-fonts.fira-code
    nil
    nixfmt
    nodejs
    nushell
    platinum-searcher
    rbenv
    ripgrep
    roswell
    rubyPackages.solargraph
    rustup
    shellcheck
    spotify
    stern
    terraform-docs
    terraform-ls
    tflint
    tfswitch
    tilt
    tmux
    tmux-xpanes
    vault
    vscode-langservers-extracted
    yaml-language-server
    zellij
    zig
    zls
    zoxide
    zstd
    git
    neofetch
    htop
    discord
    fish
    wget
    fishPlugins.done
    fishPlugins.forgit
    fishPlugins.hydro
    fishPlugins.fzf
    fzf
    nushell
    zoxide
    delta
    bat
  ];

  fonts.fontconfig.enable = true;
}
