{ pkgs, ... }:
let
  kakoune-colors = pkgs.stdenv.mkDerivation rec {
    pname = "kakoune-themes";
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
  compilation-kak = pkgs.stdenv.mkDerivation rec {
    pname = "compilation.kak";
    version = "0104272";
    src = pkgs.fetchgit {
      url = "https://codeberg.org/femi/compilation.kak.git";
      rev = version;
      sha256 = "sha256-e/V5qi0cUQ+7b9gg475lBgKtR3dz6jXEoRjkGLUhPz8=";
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
    defaultEditor = true;
    enable = true;
    colorSchemePackage = kakoune-colors;
    config = {
      colorScheme = "ef-melissa-dark";
      indentWidth = 2;
      numberLines = {
        enable = true;
        highlightCursor = true;
      };
      showMatching = true;
      tabStop = 2;
    };
    extraConfig = builtins.readFile ./extraconfig.kak;
    plugins = [
      pkgs.kakounePlugins.fzf-kak
      pkgs.kakounePlugins.auto-pairs-kak
      compilation-kak
    ];
  };
}
