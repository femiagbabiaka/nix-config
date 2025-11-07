{ pkgs, ... }:
let
  baseEmacs =
    if pkgs.stdenv.isDarwin then
      pkgs.emacs-macport
    else
      pkgs.emacs-git;

  myEmacsAttrs = baseEmacs.overrideAttrs (previousAttrs: {
    buildInputs = (previousAttrs.buildInputs or [ ]) ++ [
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
  });

  emacsWithPkgs = pkgs.emacsWithPackagesFromUsePackage {
    config = ./configuration.org;
    defaultInitFile = true;
    package = myEmacsAttrs;
    alwaysTangle = true;
    alwaysEnsure = true;

    extraEmacsPackages = epkgs:
      let
        claude-code-ide = epkgs.trivialBuild rec {
          pname = "claude-code-ide";
          version = "32d853e";
          src = pkgs.fetchFromGitHub {
            owner = "manzaltu";
            repo = "claude-code-ide.el";
            rev = version;
            hash = "sha256-OrcnUZXqRijJCgf1QE5kkPKKdWSJ4oMYt47Sn/EdQy0=";
          };
          packageRequires = with epkgs; [
            websocket
            web-server
          ];
        };
        simpc-mode = epkgs.trivialBuild rec {
          pname = "simpc-mode";
          version = "958aeb9";
          src = pkgs.fetchFromGitHub {
            owner = "rexim";
            repo = "simpc-mode";
            rev = version;
            hash = "sha256-l+6/XDhdvX6JWK61hKcOvPll4xZrNM3l87fWzBKO7BU=";
          };
        };
      in
      [
        claude-code-ide
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
