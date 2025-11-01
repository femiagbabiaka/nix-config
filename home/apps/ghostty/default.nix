{pkgs, ...}:
{
  programs.ghostty = {
    package = pkgs.ghostty-bin;
    enable = true;
    enableFishIntegration = true;
    settings = {
      font-family = "Berkeley Mono";
      font-size = 16;
      theme = "Black Metal (Bathory)";
    };
  };
}
