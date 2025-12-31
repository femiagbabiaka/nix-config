# Swaybg wallpaper configuration
# Note: swaybg is spawned at startup via niri's spawn-at-startup
# This module just ensures the package is installed and provides documentation
{ config, pkgs, lib, ... }:

{
  # The actual swaybg command is in niri's spawn-at-startup
  # To set your wallpaper:
  # 1. Copy an image to ~/Pictures/wallpaper.jpg
  # 2. Or update the path in home/apps/niri/default.nix spawn-at-startup

  home.packages = with pkgs; [
    swaybg
  ];

  # Create the Pictures/Screenshots directory for niri screenshots
  home.file."Pictures/Screenshots/.keep".text = "";
}
