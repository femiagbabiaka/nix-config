# Mako notification daemon configuration
{ config, pkgs, lib, ... }:

{
  services.mako = {
    enable = true;

    settings = {
      # General settings
      default-timeout = 5000;
      ignore-timeout = false;
      max-visible = 5;
      layer = "overlay";
      anchor = "top-right";
      sort = "-time";

      # Appearance
      font = "JetBrains Mono 10";
      background-color = "#2e3440ee";
      text-color = "#d8dee9";
      border-color = "#88c0d0";
      border-size = 2;
      border-radius = 8;
      width = 350;
      height = 150;
      margin = "10";
      padding = "15";
      icons = true;
      max-icon-size = 48;

      # Markup and format
      markup = true;
      actions = true;
      format = "<b>%s</b>\\n%b";

      # Urgency settings
      "urgency=low" = {
        border-color = "#4c566a";
        default-timeout = 3000;
      };

      "urgency=normal" = {
        border-color = "#88c0d0";
        default-timeout = 5000;
      };

      "urgency=critical" = {
        border-color = "#bf616a";
        default-timeout = 0;
        ignore-timeout = true;
      };

      "app-name=Spotify" = {
        default-timeout = 3000;
        group-by = "app-name";
      };

      "app-name=discord" = {
        default-timeout = 5000;
      };

      "app-name=Signal" = {
        default-timeout = 10000;
      };
    };
  };
}
