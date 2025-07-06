{ ... }:
{
  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal = {
          family = "FiraCode Nerd Font Mono";
        };
        size = 16;
      };
      window = {
        option_as_alt = "OnlyLeft";
      };
    };
  };
}
