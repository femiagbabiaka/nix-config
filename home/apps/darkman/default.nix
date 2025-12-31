# Darkman automatic light/dark theme switching
{ config, pkgs, lib, ... }:

let
  # Script to switch to dark mode
  darkModeScript = pkgs.writeShellScript "dark-mode" ''
    # Set GTK theme to dark
    ${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"

    # Notify the user
    ${pkgs.libnotify}/bin/notify-send -t 3000 "Dark Mode" "Switched to dark theme"
  '';

  # Script to switch to light mode
  lightModeScript = pkgs.writeShellScript "light-mode" ''
    # Set GTK theme to light
    ${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/color-scheme "'prefer-light'"

    # Notify the user
    ${pkgs.libnotify}/bin/notify-send -t 3000 "Light Mode" "Switched to light theme"
  '';
in
{
  # Install darkman
  home.packages = with pkgs; [
    darkman
    libnotify
    dconf
  ];

  # Darkman configuration
  xdg.configFile."darkman/config.yaml".text = ''
    # Latitude and longitude for America/Chicago (approximate)
    # Adjust these values for your specific location
    lat: 41.8781
    lng: -87.6298

    # Use geoclue for automatic location (if available)
    usegeoclue: false

    # Portal support for desktop integration
    portal: true

    # Run scripts from these directories
    # Scripts in dark-mode.d/ run at sunset
    # Scripts in light-mode.d/ run at sunrise
  '';

  # Create the dark-mode.d and light-mode.d directories with scripts
  xdg.dataFile = {
    "dark-mode.d/gtk-theme.sh" = {
      executable = true;
      text = ''
        #!/bin/sh
        ${darkModeScript}
      '';
    };

    "light-mode.d/gtk-theme.sh" = {
      executable = true;
      text = ''
        #!/bin/sh
        ${lightModeScript}
      '';
    };
  };

  # Set up dconf for GTK applications to respect the theme
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      color-scheme = "prefer-dark";  # Default to dark
      gtk-theme = "Adwaita";
      icon-theme = "Adwaita";
    };
  };

  # Ensure dconf service is available
  # (usually enabled automatically with GNOME apps)
}
