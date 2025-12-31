# Swaylock screen locker configuration
{ config, pkgs, lib, ... }:

{
  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;

    settings = {
      # Appearance
      color = "2e3440";
      image = "${config.home.homeDirectory}/Pictures/wallpaper.jpg";
      scaling = "fill";
      font = "JetBrains Mono";
      font-size = 24;

      # Indicator
      indicator = true;
      indicator-radius = 100;
      indicator-thickness = 10;
      indicator-caps-lock = true;

      # Colors (Nord-inspired)
      inside-color = "2e3440ee";
      inside-clear-color = "ebcb8bee";
      inside-caps-lock-color = "bf616aee";
      inside-ver-color = "5e81acee";
      inside-wrong-color = "bf616aee";

      ring-color = "4c566a";
      ring-clear-color = "ebcb8b";
      ring-caps-lock-color = "bf616a";
      ring-ver-color = "5e81ac";
      ring-wrong-color = "bf616a";

      line-color = "00000000";
      line-clear-color = "00000000";
      line-caps-lock-color = "00000000";
      line-ver-color = "00000000";
      line-wrong-color = "00000000";

      separator-color = "00000000";

      text-color = "d8dee9";
      text-clear-color = "2e3440";
      text-caps-lock-color = "eceff4";
      text-ver-color = "eceff4";
      text-wrong-color = "eceff4";

      key-hl-color = "88c0d0";
      bs-hl-color = "bf616a";
      caps-lock-key-hl-color = "ebcb8b";
      caps-lock-bs-hl-color = "bf616a";

      # Behavior
      ignore-empty-password = true;
      show-failed-attempts = true;
      daemonize = false;

      # Effects (swaylock-effects specific)
      clock = true;
      timestr = "%H:%M";
      datestr = "%Y-%m-%d";
      effect-blur = "7x5";
      effect-vignette = "0.5:0.5";
      fade-in = 0.2;
    };
  };
}
