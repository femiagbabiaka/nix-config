{
  pkgs,
  lib,
  username,
  homeDirectory,
  home-manager,
  config,
  ...
}: let
  myEmacs29 = pkgs.emacs29.overrideAttrs (finalAttrs: previousAttrs: {
    buildInputs = previousAttrs.buildInputs ++ [pkgs.tree-sitter];
  });
in {
  programs.emacs = {
    enable = true;
    package = myEmacs29;
    extraConfig = ''
      (org-babel-load-file "${homeDirectory}/.emacs.d/configuration.org")
    '';
  };

  home.file.".emacs.d/early-init.el".text = ''
    (setq package-enable-at-startup nil)
  '';
  home.file.".emacs.d/configuration.org".source =
    config.lib.file.mkOutOfStoreSymlink ./configuration.org;
  services.emacs.enable = lib.mkIf pkgs.stdenv.isLinux true;
}
