{
  pkgs,
  lib,
  username,
  homeDirectory,
  dagger,
  ...
}:
let
  myInfra = pkgs.infra.overrideAttrs (
    finalAttrs: previousAttrs: {
      version = "0.20.0";
    }
  );
  myGCSDK = pkgs.google-cloud-sdk.withExtraComponents [
    pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin
    pkgs.google-cloud-sdk.components.kubectl
  ];
in
{
  imports = [
    ./apps/emacs
    ./apps/fish
    ./apps/neovim
    ./apps/git
    ./apps/helix
    ./apps/kitty
    ./apps/alacritty
  ];

  home = {
    inherit username;
    homeDirectory = lib.mkDefault homeDirectory;
    stateVersion = "23.05";
  };

  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;
  home.packages =
    with pkgs;
    [
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
      # ghostty
      go
      golangci-lint
      golangci-lint-langserver
      gopls
      gotools
      git-crypt
      helm-ls
      helix
      jq
      k9s
      kakoune
      kubectx
      kubernetes-helm
      lldb
      myGCSDK
      myInfra
      nerd-fonts.fira-code
      nil
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
    ]
    ++ [ dagger.packages.${system}.dagger ];

  fonts.fontconfig.enable = true;
}
