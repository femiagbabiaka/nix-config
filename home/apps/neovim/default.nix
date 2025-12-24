{
  pkgs,
  lib,
  neovim-nightly-overlay,
  ...
}:
let
  initLua = builtins.readFile ./init.lua;
  nvimConfig = pkgs.writeText "init.lua" initLua;
  neovimWrapper = pkgs.symlinkJoin {
    name = "neovim-for-femi";
    paths = [ neovim-nightly-overlay.packages.${pkgs.stdenv.hostPlatform.system}.default ];
    buildInputs = [ pkgs.makeWrapper ];

    postBuild = ''
            # Wrap neovim binary
            wrapProgram $out/bin/nvim \
               --set NVIM_APPNAME "neovim-for-femi" \
               --add-flags "-u ${nvimConfig}" \
      	 --prefix PATH : "${
          lib.makeBinPath [
            pkgs.clang-tools
            pkgs.checkmake
            pkgs.clippy
            pkgs.codespell
            pkgs.eslint_d
            pkgs.gawk
            pkgs.gofumpt
            pkgs.goimports-reviser
            pkgs.golangci-lint
            pkgs.gotools
            pkgs.gopls
            pkgs.jq
            pkgs.libxml2
            pkgs.lua51Packages.lua
            pkgs.lua51Packages.luarocks
            pkgs.lua-language-server
            pkgs.mdformat
            pkgs.nodePackages.js-beautify
            pkgs.python313Packages.demjson3
            pkgs.rubyPackages.htmlbeautifier
            pkgs.shellcheck
            pkgs.stylua
            pkgs.taplo
            pkgs.tflint
            pkgs.trivy
            pkgs.yq
            pkgs.zig
            pkgs.yamllint
            pkgs.helm-ls
          ]
        }"
    '';
  };
in
{
  home.packages = lib.mkAfter [ neovimWrapper ];
}
