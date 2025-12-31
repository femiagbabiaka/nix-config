# Swayidle idle management configuration
{ config, pkgs, lib, ... }:

{
  services.swayidle = {
    enable = true;

    events = [
      {
        event = "before-sleep";
        command = "${pkgs.swaylock-effects}/bin/swaylock -f";
      }
      {
        event = "lock";
        command = "${pkgs.swaylock-effects}/bin/swaylock -f";
      }
    ];

    timeouts = [
      # Lock screen after 5 minutes of inactivity
      {
        timeout = 300;
        command = "${pkgs.swaylock-effects}/bin/swaylock -f";
      }
      # Turn off screen after 10 minutes of inactivity
      {
        timeout = 600;
        command = "${pkgs.niri}/bin/niri msg action power-off-monitors";
        resumeCommand = "${pkgs.niri}/bin/niri msg action power-on-monitors";
      }
      # Also dim screen slightly before locking (warning)
      {
        timeout = 270;
        command = "${pkgs.brightnessctl}/bin/brightnessctl -s set 30%";
        resumeCommand = "${pkgs.brightnessctl}/bin/brightnessctl -r";
      }
    ];

    systemdTarget = "graphical-session.target";
  };
}
