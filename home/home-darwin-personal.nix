{
  pkgs,
  username,
  ...
}:
{
  imports = [
    ./apps/fish
    ./apps/gitconfig
    ./apps/kitty
    ./apps/kakoune
  ];

  home = {
    inherit username;
    stateVersion = "25.05";
    shell.enableShellIntegration = true;
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
    broot
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
    fd
    fq
    gh
    go
    golangci-lint
    golangci-lint-langserver
    gopls
    git-crypt
    graphviz
    helm-ls
    helix
    jq
    jujutsu
    k9s
    kakoune
    kakoune-lsp
    kubectx
    kubernetes-helm
    lldb
    nerd-fonts.fira-code
    nil
    nixfmt-rfc-style
    nodejs
    nushell
    platinum-searcher
    rbenv
    ripgrep
    roswell
    rubyPackages.solargraph
    rustup
    shellcheck
    stern
    terraform-docs
    terraform-ls
    tflint
    tfswitch
    tilt
    vscode-langservers-extracted
    yaml-language-server
    zig
    zls
    zoxide
    zstd
    git
    neofetch
    htop
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
    glibtool
    vscode-langservers-extracted
  ]; # ++ [ linux-pkgs.kak-tree-sitter ];

  fonts.fontconfig.enable = true;
}
