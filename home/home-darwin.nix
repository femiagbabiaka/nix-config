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
