# Waybar configuration for niri
{ config, pkgs, lib, ... }:

{
  programs.waybar = {
    enable = true;
    systemd.enable = true;

    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 30;
        spacing = 4;

        modules-left = [
          "niri/workspaces"
          "niri/window"
        ];

        modules-center = [
          "clock"
        ];

        modules-right = [
          "idle_inhibitor"
          "pulseaudio"
          "network"
          "bluetooth"
          "battery"
          "tray"
        ];

        "niri/workspaces" = {
          format = "{icon}";
          format-icons = {
            active = "";
            default = "";
            urgent = "";
          };
        };

        "niri/window" = {
          format = "{}";
          max-length = 50;
          rewrite = {
            "(.*) - Mozilla Firefox" = " $1";
            "(.*) - Brave" = " $1";
            "(.*) - Chromium" = " $1";
          };
        };

        clock = {
          format = "{:%H:%M}";
          format-alt = "{:%Y-%m-%d %H:%M:%S}";
          tooltip-format = "<tt><small>{calendar}</small></tt>";
          calendar = {
            mode = "year";
            mode-mon-col = 3;
            weeks-pos = "right";
            on-scroll = 1;
            format = {
              months = "<span color='#ffead3'><b>{}</b></span>";
              days = "<span color='#ecc6d9'><b>{}</b></span>";
              weeks = "<span color='#99ffdd'><b>W{}</b></span>";
              weekdays = "<span color='#ffcc66'><b>{}</b></span>";
              today = "<span color='#ff6699'><b><u>{}</u></b></span>";
            };
          };
        };

        idle_inhibitor = {
          format = "{icon}";
          format-icons = {
            activated = "";
            deactivated = "";
          };
          tooltip-format-activated = "Idle inhibitor: ON";
          tooltip-format-deactivated = "Idle inhibitor: OFF";
        };

        pulseaudio = {
          format = "{icon} {volume}%";
          format-bluetooth = "{icon} {volume}%";
          format-bluetooth-muted = " {icon}";
          format-muted = " muted";
          format-icons = {
            headphone = "";
            hands-free = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = [ "" "" "" ];
          };
          on-click = "pavucontrol";
          on-click-right = "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
        };

        network = {
          format-wifi = " {signalStrength}%";
          format-ethernet = " {ipaddr}";
          format-linked = " {ifname}";
          format-disconnected = "⚠ Disconnected";
          tooltip-format = "{ifname}: {ipaddr}/{cidr}\n{essid}";
          on-click = "nm-connection-editor";
        };

        bluetooth = {
          format = " {status}";
          format-connected = " {device_alias}";
          format-connected-battery = " {device_alias} {device_battery_percentage}%";
          tooltip-format = "{controller_alias}\t{controller_address}\n\n{num_connections} connected";
          tooltip-format-connected = "{controller_alias}\t{controller_address}\n\n{num_connections} connected\n\n{device_enumerate}";
          tooltip-format-enumerate-connected = "{device_alias}\t{device_address}";
          tooltip-format-enumerate-connected-battery = "{device_alias}\t{device_address}\t{device_battery_percentage}%";
          on-click = "blueman-manager";
        };

        battery = {
          states = {
            good = 80;
            warning = 30;
            critical = 15;
          };
          format = "{icon} {capacity}%";
          format-charging = " {capacity}%";
          format-plugged = " {capacity}%";
          format-alt = "{icon} {time}";
          format-icons = [ "" "" "" "" "" ];
        };

        tray = {
          icon-size = 18;
          spacing = 10;
        };
      };
    };

    style = ''
      * {
        font-family: "JetBrains Mono", "Font Awesome 6 Free", monospace;
        font-size: 13px;
        min-height: 0;
      }

      window#waybar {
        background-color: rgba(43, 48, 59, 0.9);
        color: #d8dee9;
        border-bottom: 2px solid rgba(136, 192, 208, 0.5);
      }

      window#waybar.hidden {
        opacity: 0.2;
      }

      #workspaces button {
        padding: 0 8px;
        background-color: transparent;
        color: #d8dee9;
        border-radius: 0;
        border-bottom: 2px solid transparent;
      }

      #workspaces button:hover {
        background: rgba(136, 192, 208, 0.2);
      }

      #workspaces button.active {
        background-color: rgba(136, 192, 208, 0.3);
        border-bottom: 2px solid #88c0d0;
      }

      #workspaces button.urgent {
        background-color: rgba(191, 97, 106, 0.5);
      }

      #window {
        padding: 0 10px;
        color: #81a1c1;
      }

      #clock,
      #battery,
      #pulseaudio,
      #network,
      #bluetooth,
      #idle_inhibitor,
      #tray {
        padding: 0 10px;
        margin: 0 4px;
        color: #d8dee9;
      }

      #clock {
        font-weight: bold;
        color: #88c0d0;
      }

      #battery {
        color: #a3be8c;
      }

      #battery.charging, #battery.plugged {
        color: #a3be8c;
      }

      #battery.warning:not(.charging) {
        background-color: rgba(235, 203, 139, 0.3);
        color: #ebcb8b;
      }

      #battery.critical:not(.charging) {
        background-color: rgba(191, 97, 106, 0.5);
        color: #bf616a;
        animation-name: blink;
        animation-duration: 0.5s;
        animation-timing-function: linear;
        animation-iteration-count: infinite;
        animation-direction: alternate;
      }

      @keyframes blink {
        to {
          background-color: rgba(191, 97, 106, 0.8);
          color: #eceff4;
        }
      }

      #pulseaudio {
        color: #b48ead;
      }

      #pulseaudio.muted {
        color: #4c566a;
      }

      #network {
        color: #8fbcbb;
      }

      #network.disconnected {
        color: #bf616a;
      }

      #bluetooth {
        color: #81a1c1;
      }

      #bluetooth.connected {
        color: #88c0d0;
      }

      #idle_inhibitor {
        color: #4c566a;
      }

      #idle_inhibitor.activated {
        color: #ebcb8b;
      }

      #tray {
        padding: 0 10px;
      }

      #tray > .passive {
        -gtk-icon-effect: dim;
      }

      #tray > .needs-attention {
        -gtk-icon-effect: highlight;
        background-color: rgba(191, 97, 106, 0.5);
      }
    '';
  };
}
