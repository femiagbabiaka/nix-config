{ pkgs, ... }:
let
  baseEmacs =
    if pkgs.stdenv.isDarwin then
      pkgs.emacs-macport
    else
      pkgs.emacs-git;

  myEmacsAttrs = baseEmacs.overrideAttrs (previousAttrs: {
    nativeBuildInputs = (previousAttrs.nativeBuildInputs or [ ]) ++ [
      pkgs.git
    ];
    buildInputs = (previousAttrs.buildInputs or [ ]) ++ [
      pkgs.jansson
      pkgs.powerline-fonts
      pkgs.claude-code
      pkgs.libtool
      pkgs.gnulib
      pkgs.nix
      pkgs.nil
      pkgs.delve
      pkgs.go
      pkgs.golangci-lint
      pkgs.golangci-lint-langserver
      pkgs.gopls
      pkgs.helm-ls
      pkgs.nixfmt-rfc-style
      pkgs.kubernetes-helm
      pkgs.rust-analyzer
      pkgs.terraform-ls
      pkgs.vscode-langservers-extracted
      pkgs.yaml-language-server
      pkgs.zls
      pkgs.tflint
      pkgs.ripgrep
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
        lean4-mode = epkgs.trivialBuild rec {
          pname = "lean4-mode";
          version = "1388f9d";
          src = pkgs.fetchFromGitHub {
            owner = "leanprover-community";
            repo = "lean4-mode";
            rev = version;
            hash = "sha256-6XFcyqSTx1CwNWqQvIc25cuQMwh3YXnbgr5cDiOCxBk=";
          };
          packageRequires = with epkgs; [
            dash
            lsp-mode
            magit-section
         ];
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
        epkgs.treesit-grammars.with-all-grammars
        jj-mode
        lean4-mode
        pkgs.claude-code
        pkgs.delve
        pkgs.go
        pkgs.golangci-lint
        pkgs.golangci-lint-langserver
        pkgs.gopls
        pkgs.helm-ls
        pkgs.kubernetes-helm
        pkgs.nil
        pkgs.nix
        pkgs.nixfmt-rfc-style
        pkgs.ripgrep
        pkgs.rust-analyzer
        pkgs.terraform-ls
        pkgs.tflint
        pkgs.vscode-langservers-extracted
        pkgs.yaml-language-server
        pkgs.zls
        poly-helm-mode
        simpc-mode
      ];

    override = final: prev: { # this is literally _just_ for forge, which needs git at runtime
      trivialBuild = args:
        if args.pname == "default" then
          prev.trivialBuild (args // {
            nativeBuildInputs = (args.nativeBuildInputs or [ ]) ++ [ pkgs.git ];
          })
        else
          prev.trivialBuild args;
    };
  };
in
{
  programs.emacs = {
    enable = true;
    package = emacsWithPkgs;
  };

  # This writes the early-init.el to ~/.config/emacs/early-init.el
  xdg.configFile."emacs/early-init.el".text = ''
    ;; Increase GC threshold to infinity during startup
    (setq gc-cons-threshold most-positive-fixnum)
  '';

}
