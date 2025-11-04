{ pkgs, ... }:
let
  claude-code-ide-el = pkgs.emacsPackages.trivialBuild rec {
    pname = "claude-code-ide.el";
    version = "32d853e";
    src = pkgs.fetchFromGitHub {
      owner = "manzaltu";
      repo = "claude-code-ide.el";
      rev = version;
      hash = "sha256-OrcnUZXqRijJCgf1QE5kkPKKdWSJ4oMYt47Sn/EdQy0=";
    };
    packageRequires = with pkgs.emacsPackages; [
      websocket
      web-server
    ];
  };
  simpc-mode = pkgs.emacsPackages.trivialBuild rec {
    pname = "simpc-mode";
    version = "958aeb9";
    src = pkgs.fetchFromGitHub {
      owner = "rexim";
      repo = "simpc-mode";
      rev = version;
      hash = "sha256-l+6/XDhdvX6JWK61hKcOvPll4xZrNM3l87fWzBKO7BU=";
    };
  };
  myEmacsAttrs = pkgs.emacs-git-pgtk.overrideAttrs (previousAttrs: {
    buildInputs = previousAttrs.buildInputs ++ [
      pkgs.tree-sitter
      pkgs.jansson
      pkgs.powerline-fonts
      pkgs.claude-code
      pkgs.libtool
      pkgs.gnulib
      pkgs.tree-sitter-grammars.tree-sitter-nix
      pkgs.tree-sitter-grammars.tree-sitter-rust
      pkgs.tree-sitter-grammars.tree-sitter-go
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

    extraEmacsPackages = epkgs: [
      claude-code-ide-el
      simpc-mode
      epkgs.treesit-grammars.with-all-grammars
    ];
  };
in
{
  programs.emacs = {
    enable = true;
    package = emacsWithPkgs;
  };

}
