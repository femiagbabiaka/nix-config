{ pkgs
, username
, lib ? pkgs.lib
, extraImports ? [ ]
, extraPackages ? [ ]
, homeStateVersion ? "23.05"
, homeExtraConfig ? { }
, configOverrides ? { }
, ...
}:
let
  inherit (lib) unique;

  sharedImports = [
    ../apps/fish
    ../apps/gitconfig
    ../apps/kitty
    ../apps/neovim
    ../apps/emacs
    ../apps/ghostty
  ];

  basePackages =
    with pkgs;
    [
      aider-chat-with-playwright
      ansible
      automake
      awscli2
      bat
      broot
      cmake
      colima
      coreutils
      crane
      curl
      delta
      delve
      dive
      docker
      docker-compose
      dockerfile-language-server
      fd
      fish
      fishPlugins.done
      fishPlugins.hydro
      fq
      fzf
      gh
      git
      git-crypt
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
      kubernetes-helm
      lima
      lldb
      mosh
      neofetch
      nerd-fonts.fira-code
      nil
      nixfmt-rfc-style
      nodejs
      nushell
      platinum-searcher
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
    ];
in
{
  imports = sharedImports ++ extraImports;

  home =
    {
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
