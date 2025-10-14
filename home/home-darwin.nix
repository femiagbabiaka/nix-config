{
  pkgs,
  username,
  dagger,
  ...
}:
let
  myInfra = pkgs.infra.overrideAttrs (
    finalAttrs: previousAttrs: rec {
      version = "0.20.0";
      src = pkgs.fetchFromGitHub {
        owner = "infrahq";
        repo = "infra";
        rev = "v${version}";
        sha256 = "sha256-uz4wimhOfeHSL949m+biIhjfDwwEGnTiJWaz/r3Rsko=";
      };
    }
  );
  myGCSDK = pkgs.google-cloud-sdk.withExtraComponents [
    pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin
    pkgs.google-cloud-sdk.components.kubectl
  ];
in
{
  imports = [
    ./apps/fish
    ./apps/gitconfig
    ./apps/helix
    ./apps/kitty
    ./apps/alacritty
    ./apps/neovim
    ./apps/emacs
  ];

  home = {
    inherit username;
    stateVersion = "23.05";
  };

  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;
  home.packages =
    with pkgs;
    [
      aider-chat-with-playwright
      ansible
      automake
      awscli2
      bat
      bat
      broot
      cloud-provider-kind
      cmake
      claude-code
      colima
      coreutils
      crane
      ctlptl
      curl
      delta
      delve
      discord
      dive
      docker
      docker-compose
      docker-credential-helpers
      dockerfile-language-server-nodejs
      fastly
      fd
      fish
      fishPlugins.done
      fishPlugins.hydro
      fluxcd
      fq
      fzf
      gh
      git
      git-crypt
      git-lfs
      go
      golangci-lint
      golangci-lint-langserver
      gopls
      graphviz
      helix
      helm-ls
      htop
      jq
      jujutsu
      k9s
      kakoune-lsp
      kind
      kubeswitch
      kubernetes-helm
      lima
      lldb
      myGCSDK
      myInfra
      neofetch
      nerd-fonts.fira-code
      nil
      nixfmt-rfc-style
      nodejs
      nushell
      platinum-searcher
      presenterm
      rbenv
      ripgrep
      rustup
      shellcheck
      spotify
      stern
      terraform-docs
      terraform-ls
      tflint
      tfswitch
      tilt
      uv
      vault
      vscode-langservers-extracted
      wget
      yaml-language-server
      zellij
      zig
      zls
      zoxide
      zoxide
      zstd
    ]
    ++ [ dagger.packages.${system}.dagger ];

  fonts.fontconfig.enable = true;
}
