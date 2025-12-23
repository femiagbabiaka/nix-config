{ pkgs, ... }:
let
  kakoune-colors = pkgs.stdenv.mkDerivation rec {
    pname = "kakoune-ts-themes";
    version = "205ec3f2";
    src = pkgs.fetchFromGitHub {
      owner = "anhsirk0";
      repo = "kakoune-themes";
      rev = version;
      sha256 = "sha256-6ZJuhxMozTKtS5gTmFcVS99+stCaxXcXlqCdlvm5VE8=";
    };
    installPhase = ''
      runHook preInstall

      target=$out/share/kak/autoload/plugins/${pname}
      mkdir -p $out/share/kak/autoload/plugins
      cp -r . $target

      runHook postInstall
    '';
  };
in
{
  programs.kakoune = {
    enable = true;
    extraConfig = builtins.readFile ./extraconfig.kak;
    plugins = [
      pkgs.kakounePlugins.fzf-kak
      pkgs.kakounePlugins.auto-pairs-kak
    ];
  };
}
