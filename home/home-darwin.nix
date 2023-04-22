{ pkgs, lib, username, homeDirectory, home-manager, config, ... }: {
  home = {
    inherit username;
    homeDirectory = lib.mkDefault homeDirectory;
    stateVersion = "23.05";
  };

  home.packages = with pkgs; [
    ansible
    automake
    cmake
    colima
    coreutils
    curl
    fastly
    fd
    fq
    gh
    gopls
    infra
    jq
    k9s
    kubectx
    platinum-searcher
    rbenv
    shellcheck
    terraform-docs
    tflint
    terraform-ls
    tfswitch
    tmux
    tmux-xpanes
    zoxide
  ];

  programs.nushell.enable = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit;
    extraConfig = ''
      (org-babel-load-file "${homeDirectory}/.emacs.d/configuration.org")
    '';
  };

  home.file.".emacs.d/early-init.el".text = ''
    (setq package-enable-at-startup nil)
  '';
  home.file.".emacs.d/configuration.org".source =
    config.lib.file.mkOutOfStoreSymlink ./emacs/configuration.org;
  programs.home-manager.enable = true;
  nixpkgs.config.allowUnfree = true;
  programs.fish = {
    enable = true;
    interactiveShellInit = builtins.readFile ./fish/init.fish;
    shellAliases = {
      vim = "emacsclient -c";
      gc = "git commit";
      gp = "git push";
    };
    plugins = [
      {
        name = "nix-env.fish";
        src = pkgs.fetchFromGitHub {
          owner = "lilyball";
          repo = "nix-env.fish";
          rev = "7b65bd228429e852c8fdfa07601159130a818cfa";
          sha256 = "RG/0rfhgq6aEKNZ0XwIqOaZ6K5S4+/Y5EEMnIdtfPhk=";
        };
      }
    ];
  };

  programs.neovim = {
    enable = true;
    vimAlias = true;
    plugins = with pkgs.vimPlugins; [ vim-nix vim-sensible ];
    extraConfig = ''
      set number
      set cc=80
      set list
      set listchars=tab:→\ ,space:·,nbsp:␣,trail:•,eol:¶,precedes:«,extends:»
      if &diff
        colorscheme blue
      endif
    '';
  };

  programs.git = {
    enable = true;
    userName = "Femi Agbabiaka";
    userEmail = "femi@femiagbabiaka.xyz";
    delta = { enable = true; };
  };
}
