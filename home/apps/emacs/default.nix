{ pkgs, ... }:
let
  myEmacsAttrs = pkgs.emacs-git-pgtk.overrideAttrs (previousAttrs: {
    buildInputs = previousAttrs.buildInputs ++ [
      pkgs.tree-sitter
      pkgs.jansson
      pkgs.powerline-fonts
    ];
    patches =
      (previousAttrs.patches or [ ])
      # Only add the patches when condition is true
      ++ (
        if pkgs.stdenv.isDarwin then
          [
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-31/round-undecorated-frame.patch";
              sha256 = "WWLg7xUqSa656JnzyUJTfxqyYB/4MCAiiiZUjMOqjuY=";
            })
            (pkgs.fetchpatch {
              url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-31/system-appearance.patch";
              sha256 = "4+2U+4+2tpuaThNJfZOjy1JPnneGcsoge9r+WpgNDko=";
            })
          ]
        else
          [ ]
      );
  });

  emacsWithPkgs = pkgs.emacsWithPackagesFromUsePackage {
    config = ./configuration.org;
    defaultInitFile = true;
    package = myEmacsAttrs;
    alwaysTangle = true;
    alwaysEnsure = true;
  };
in
{
  programs.emacs = {
    enable = true;
    package = emacsWithPkgs;
  };

}
