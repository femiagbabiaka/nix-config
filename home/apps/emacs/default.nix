{ pkgs, ... }:
let
  baseEmacs = if pkgs.stdenv.isDarwin then pkgs.emacs-macport else pkgs.emacs-igc;

  myEmacsAttrs = baseEmacs.overrideAttrs (previousAttrs: {
    nativeBuildInputs = (previousAttrs.nativeBuildInputs or [ ]) ++ [
      pkgs.git
    ];
  });

  emacsWithPkgs = pkgs.emacsWithPackagesFromUsePackage {
    config = ./configuration.org;
    defaultInitFile = true;
    package = myEmacsAttrs;
    alwaysTangle = true;
    alwaysEnsure = true;

    extraEmacsPackages =
      epkgs:
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
        nael = epkgs.trivialBuild rec {
          pname = "nael";
          version = "6f83a50dce";
          src = pkgs.fetchFromGitea {
            domain = "codeberg.org";
            owner = "mekeor";
            repo = "nael";
            rev = version;
            hash = "sha256-8K7MXP60ppbZV6VvMnBp0/EOfw/YqIo9iM4oBrrGMis=";
          };
          sourceRoot = "${src.name}/nael";
        };
        jj-mode = epkgs.trivialBuild rec {
          pname = "jj-mode";
          version = "f35439f";
          src = pkgs.fetchFromGitHub {
            owner = "bolivier";
            repo = "jj-mode.el";
            rev = version;
            hash = "sha256-jgJnXJRrMhZBx1sSVk5vHAn9dolCtl8pS4y+vq9L8VQ=";
          };
          packageRequires = with epkgs; [
            magit
          ];
        };
        poly-helm-mode = epkgs.trivialBuild rec {
          pname = "poly-helm-mode";
          version = "b30c522";
          src = pkgs.fetchFromGitHub {
            owner = "femiagbabiaka";
            repo = "poly-helm-mode";
            rev = version;
            hash = "sha256-rjDki/XZJLX7Qi9rrlbJPFMT5dIQj37ONAhulykdEVM=";
          };
          packageRequires = with epkgs; [
            polymode
            yaml-mode
          ];
        };
      in
      [
        claude-code-ide
        epkgs.esup
        (epkgs.treesit-grammars.with-grammars (
          grammars: builtins.attrValues (builtins.removeAttrs grammars [ "tree-sitter-razor" ])
        ))
        jj-mode
        nael
        poly-helm-mode
        simpc-mode
      ];

    override = (
      final: prev: {
        # this is literally _just_ for forge, which needs git at runtime
        trivialBuild =
          args:
          if args.pname == "default" then
            prev.trivialBuild (
              args
              // {
                nativeBuildInputs = (args.nativeBuildInputs or [ ]) ++ [ pkgs.git ];
              }
            )
          else
            prev.trivialBuild args;

        org = prev.org.overrideAttrs (old: {
          version = "9.7.39";
          src = pkgs.fetchurl {
            url = "https://elpa.gnu.org/packages/org-9.7.39.tar";
            sha256 = "sha256-vC/pU4haCCU1m4ay3XDDDIy88/Z8ds+u46JhTRAE5fk=";
          };
        });
      }
    );
  };
in
{
  programs.emacs = {
    enable = true;
    package = emacsWithPkgs;
  };

  services.emacs.enable = true;

  # This writes the early-init.el to ~/.emacs.d/early-init.el
  home.file.".emacs.d/early-init.el".text = ''
    ;; -*- lexical-binding: t -*-
    ;; Increase GC threshold to infinity during startup
    (setq gc-cons-threshold most-positive-fixnum)

    ;; Disable UI elements before they're rendered (faster than init.el)
    (push '(tool-bar-lines . 0) default-frame-alist)
    (push '(menu-bar-lines . 0) default-frame-alist)
    (push '(vertical-scroll-bars) default-frame-alist)

    ;; Prevent package.el from loading packages at startup
    (setq package-enable-at-startup nil)

    ;; Prevent visual flash of default theme
    (push '(ns-transparent-titlebar . t) default-frame-alist)
    (push '(ns-appearance . dark) default-frame-alist)

    ;; Disable file-name-handler-alist during startup (restores automatically)
    (defvar my/file-name-handler-alist file-name-handler-alist)
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda () (setq file-name-handler-alist my/file-name-handler-alist)))
  '';

}
