{
  pkgs,
  lib,
  ...
}:
let
  initLua = builtins.readFile ./init.lua;
  nvimConfig = pkgs.writeText "init.lua" initLua;
  neovimWrapper = pkgs.symlinkJoin {
    name = "neovim-for-femi";
    paths = [ pkgs.neovim ];
    buildInputs = [ pkgs.makeWrapper ];

    postBuild = ''
      # Wrap neovim binary
      wrapProgram $out/bin/nvim \
         --set NVIM_APPNAME "neovim-for-femi" \
         --add-flags "-u ${nvimConfig}" \
	 --prefix PATH : "${lib.makeBinPath [
	    pkgs.checkmake
	    pkgs.clippy
	    pkgs.codespell
	    pkgs.eslint_d
	    pkgs.gawk
	    pkgs.gofumpt
	    pkgs.goimports-reviser
	    pkgs.golangci-lint
	    pkgs.gotools
	    pkgs.jq
	    pkgs.libxml2
	    pkgs.lua51Packages.lua
	    pkgs.lua51Packages.luarocks
	    pkgs.mdformat
	    pkgs.nodePackages.js-beautify
	    pkgs.python313Packages.demjson3
	    pkgs.rubyPackages.htmlbeautifier
	    pkgs.rustfmt
	    pkgs.shellcheck
	    pkgs.stylua
	    pkgs.taplo
	    pkgs.tflint
	    pkgs.trivy
	    pkgs.yq
	    pkgs.zig
        pkgs.yamllint
	 ]}"
    '';
  };
in
{
  home.packages = lib.mkAfter [ neovimWrapper ];
}
