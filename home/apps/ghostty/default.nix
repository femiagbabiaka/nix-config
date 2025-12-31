{pkgs, ...}:
{
  programs.ghostty = {
    package = if pkgs.stdenv.isDarwin then pkgs.ghostty-bin else pkgs.ghostty;
    enable = true;
    enableFishIntegration = true;
    settings = {
      font-family = "Berkeley Mono";
      font-size = 16;
      theme = "Material";
    };
  };
}
