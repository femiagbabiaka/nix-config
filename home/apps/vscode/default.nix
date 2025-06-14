{
  pkgs,
  ...
}:
{
  programs.vscode = {
    enable = true;
    profiles = {
      default = {
        userSettings = {
          "extensions" = {
            "experimental" = {
              "affinity" = {
                "asvetliakov.vscode-neovim" = 1;
              };
            };
          };
          "editor.fontFamily" = "Go Mono for Powerline";
          "editor.fontSize" = 13;
          "editor.formatOnPaste" = true;
          "editor.formatOnSave" = true;
          "editor.fontLigatures" = true;
          "files.autoSave" = "afterDelay";
          "nix.enableLanguageServer" = true;
          "nix.serverPath" = "nil";
          "nix.serverSettings" = {
            "nil" = {
              "formatting" = {
                "command" = [ "nixfmt" ];
              };
            };
          };
          "files.readonlyInclude" = {
            "**/.cargo/registry/src/**/*.rs" = true;
            "**/.cargo/git/checkouts/**/*.rs" = true;
            "**/lib/rustlib/src/rust/library/**/*.rs" = true;
          };
          "vscode-neovim" = {
            "neovimExecutablePaths" = {
              "darwin" = "${pkgs.neovim}/bin/nvim";
            };
          };
          "[terraform]" = {
            "editor.defaultFormatter" = "hashicorp.terraform";
            "editor.formatOnSave" = true;
            "editor.formatOnSaveMode" = "file";
          };
          "[terraform-vars]" = {
            "editor.defaultFormatter" = "hashicorp.terraform";
            "editor.formatOnSave" = true;
            "editor.formatOnSaveMode" = "file";
          };
          "gitlens.ai.ollama.url" = "http://localhost:11434";
          "workbench.colorTheme" = "Ayu Mirage";
        };
        extensions = with pkgs; [
          vscode-extensions.rust-lang.rust-analyzer
          vscode-extensions.hashicorp.terraform
          vscode-extensions.tim-koehler.helm-intellisense
          vscode-extensions.teabyii.ayu
          vscode-extensions.jnoortheen.nix-ide
          vscode-extensions.rooveterinaryinc.roo-cline
          vscode-extensions.asvetliakov.vscode-neovim
          vscode-extensions.ziglang.vscode-zig
          vscode-extensions.eamodio.gitlens
          vscode-extensions.golang.go
        ];
      };
    };
  };
}
