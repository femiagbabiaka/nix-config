{ pkgs, ... }:
let
  myEmacsAttrs = pkgs.emacs-git-pgtk.overrideAttrs (previousAttrs: {
    buildInputs = previousAttrs.buildInputs
      ++ [ pkgs.tree-sitter pkgs.jansson ];
    patches = (previousAttrs.patches or [ ])
    # Only add the patches when condition is true
      ++ (if pkgs.stdenv.isDarwin then [
        # Fix OS window role (needed for window managers like yabai)
        (pkgs.fetchpatch {
          url =
            "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
          sha256 = "+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
        })
        (pkgs.fetchpatch {
          url =
            "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-31/round-undecorated-frame.patch";
          sha256 = "WWLg7xUqSa656JnzyUJTfxqyYB/4MCAiiiZUjMOqjuY=";
        })
        (pkgs.fetchpatch {
          url =
            "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/refs/heads/master/patches/emacs-31/system-appearance.patch";
          sha256 = "4+2U+4+2tpuaThNJfZOjy1JPnneGcsoge9r+WpgNDko=";
        })
      ] else
        [ ]);
  });

  myEmacs = (myEmacsAttrs.override { withNativeCompilation = false; });
in {
  programs.emacs = {
    enable = true;
    package = myEmacs;
  };

}
