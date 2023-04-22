{ pkgs, lib, username, homeDirectory, home-manager, config, ... }: {
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
    config.lib.file.mkOutOfStoreSymlink ./configuration.org;
}
