{ ... }:
{
  programs.alacritty = {
    enable = true;
    settings = {
      font = {
        normal = {
          family = "Go Mono for Powerline";
          style = "Regular";
        };
        size = 16;
      };
      window = {
        option_as_alt = "OnlyLeft";
      };
    };
  };
}
