# Niri scrollable-tiling Wayland compositor configuration
{ config, pkgs, ... }:

{
  programs.niri.settings = {
    # Skip the hotkey overlay on startup (it's available via Mod+Shift+/)
    hotkey-overlay.skip-at-startup = true;

    # Screenshot path
    screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";

    # Prefer server-side decorations
    prefer-no-csd = true;

    # Environment variables for spawned processes
    environment = {
      NIXOS_OZONE_WL = "1";
      QT_QPA_PLATFORM = "wayland";
      MOZ_ENABLE_WAYLAND = "1";
    };

    # Input configuration
    input = {
      keyboard.xkb = {
        layout = "us";
        options = "ctrl:nocaps";
      };

      touchpad = {
        tap = true;
        natural-scroll = true;
        click-method = "clickfinger";
        dwt = true;  # Disable while typing
      };

      mouse = {
        natural-scroll = true;
      };

      focus-follows-mouse.enable = false;
      warp-mouse-to-focus.enable = false;
      workspace-auto-back-and-forth = true;
    };

    # Output configuration (placeholder - adjust for your monitors)
    outputs = {
      # Example for laptop display - uncomment and adjust as needed
      # "eDP-1" = {
      #   scale = 1.5;
      #   position = { x = 0; y = 0; };
      # };
    };

    # Layout configuration
    layout = {
      gaps = 8;

      # Default window width: half screen
      default-column-width = { proportion = 0.5; };

      # Preset column widths for cycling
      preset-column-widths = [
        { proportion = 1.0 / 3.0; }
        { proportion = 0.5; }
        { proportion = 2.0 / 3.0; }
      ];

      # Focus ring (around the focused window)
      focus-ring = {
        enable = true;
        width = 2;
        active.color = "#88c0d0";   # Nord frost blue
        inactive.color = "#4c566a"; # Nord gray
      };

      # Border (inside the window, takes space)
      border = {
        enable = false;
      };

      # Center focused column on overflow
      center-focused-column = "on-overflow";
    };

    # Cursor configuration
    cursor = {
      size = 24;
      theme = "Adwaita";
    };

    # Spawn at startup
    # Note: waybar is started via systemd (programs.waybar.systemd.enable)
    spawn-at-startup = [
      # Import Wayland environment into systemd user session so services like emacs get WAYLAND_DISPLAY
      # DISPLAY is set automatically by niri's native xwayland-satellite integration
      { command = [ "systemctl" "--user" "import-environment" "WAYLAND_DISPLAY" "XDG_CURRENT_DESKTOP" "DISPLAY" ]; }
      # Notify systemd that the graphical session is ready (restarts services that need it)
      { command = [ "systemctl" "--user" "start" "graphical-session.target" ]; }
      { command = [ "mako" ]; }
      { command = [ "nm-applet" "--indicator" ]; }
      { command = [ "blueman-applet" ]; }
      { command = [ "${pkgs.swaybg}/bin/swaybg" "-i" "${config.home.homeDirectory}/Pictures/wallpaper.jpg" "-m" "fill" ]; }
      { command = [ "swayidle" "-w" ]; }
      { command = [ "darkman" "run" ]; }
      # Reset waybar restart counter on startup
      { command = [ "systemctl" "--user" "reset-failed" "waybar.service" ]; }
    ];

    # Keybindings
    binds = {
      # Terminal
      "Mod+Return".action.spawn = "kitty";

      # Application launcher
      "Mod+D".action.spawn = "fuzzel";

      # Lock screen
      "Mod+Escape".action.spawn = [ "swaylock" "-f" ];

      # Close window
      "Mod+Shift+Q".action.close-window = [];

      # Quit niri
      "Mod+Shift+E".action.quit = { skip-confirmation = false; };

      # Toggle fullscreen
      "Mod+F".action.fullscreen-window = [];

      # Toggle floating
      "Mod+Shift+Space".action.toggle-window-floating = [];

      # Overview
      "Mod+Tab".action.toggle-overview = [];

      # Show hotkey overlay
      "Mod+Shift+Slash".action.show-hotkey-overlay = [];

      # Focus navigation (vim-style)
      "Mod+H".action.focus-column-left = [];
      "Mod+L".action.focus-column-right = [];
      "Mod+J".action.focus-window-down = [];
      "Mod+K".action.focus-window-up = [];

      # Arrow key navigation
      "Mod+Left".action.focus-column-left = [];
      "Mod+Right".action.focus-column-right = [];
      "Mod+Down".action.focus-window-down = [];
      "Mod+Up".action.focus-window-up = [];

      # Move windows (vim-style)
      "Mod+Shift+H".action.move-column-left = [];
      "Mod+Shift+L".action.move-column-right = [];
      "Mod+Shift+J".action.move-window-down = [];
      "Mod+Shift+K".action.move-window-up = [];

      # Move windows (arrow keys)
      "Mod+Shift+Left".action.move-column-left = [];
      "Mod+Shift+Right".action.move-column-right = [];
      "Mod+Shift+Down".action.move-window-down = [];
      "Mod+Shift+Up".action.move-window-up = [];

      # Monitor focus
      "Mod+Ctrl+H".action.focus-monitor-left = [];
      "Mod+Ctrl+L".action.focus-monitor-right = [];
      "Mod+Ctrl+J".action.focus-monitor-down = [];
      "Mod+Ctrl+K".action.focus-monitor-up = [];

      # Move window to monitor
      "Mod+Ctrl+Shift+H".action.move-column-to-monitor-left = [];
      "Mod+Ctrl+Shift+L".action.move-column-to-monitor-right = [];
      "Mod+Ctrl+Shift+J".action.move-column-to-monitor-down = [];
      "Mod+Ctrl+Shift+K".action.move-column-to-monitor-up = [];

      # Workspace navigation
      "Mod+1".action.focus-workspace = 1;
      "Mod+2".action.focus-workspace = 2;
      "Mod+3".action.focus-workspace = 3;
      "Mod+4".action.focus-workspace = 4;
      "Mod+5".action.focus-workspace = 5;
      "Mod+6".action.focus-workspace = 6;
      "Mod+7".action.focus-workspace = 7;
      "Mod+8".action.focus-workspace = 8;
      "Mod+9".action.focus-workspace = 9;

      # Move window to workspace
      "Mod+Shift+1".action.move-column-to-workspace = 1;
      "Mod+Shift+2".action.move-column-to-workspace = 2;
      "Mod+Shift+3".action.move-column-to-workspace = 3;
      "Mod+Shift+4".action.move-column-to-workspace = 4;
      "Mod+Shift+5".action.move-column-to-workspace = 5;
      "Mod+Shift+6".action.move-column-to-workspace = 6;
      "Mod+Shift+7".action.move-column-to-workspace = 7;
      "Mod+Shift+8".action.move-column-to-workspace = 8;
      "Mod+Shift+9".action.move-column-to-workspace = 9;

      # Workspace scrolling
      "Mod+Page_Up".action.focus-workspace-up = [];
      "Mod+Page_Down".action.focus-workspace-down = [];
      "Mod+Shift+Page_Up".action.move-column-to-workspace-up = [];
      "Mod+Shift+Page_Down".action.move-column-to-workspace-down = [];

      # Column width
      "Mod+R".action.switch-preset-column-width = [];
      "Mod+Minus".action.set-column-width = "-10%";
      "Mod+Equal".action.set-column-width = "+10%";

      # Window height
      "Mod+Shift+Minus".action.set-window-height = "-10%";
      "Mod+Shift+Equal".action.set-window-height = "+10%";

      # Consume/expel windows from columns
      "Mod+BracketLeft".action.consume-window-into-column = [];
      "Mod+BracketRight".action.expel-window-from-column = [];

      # Maximize column
      "Mod+M".action.maximize-column = [];

      # Center column
      "Mod+C".action.center-column = [];

      # Screenshots
      "Print".action.screenshot = [];
      "Mod+Print".action.screenshot-window = [];
      "Mod+Shift+Print".action.screenshot-screen = [];

      # Volume control
      "XF86AudioRaiseVolume" = {
        allow-when-locked = true;
        action.spawn = [ "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%+" ];
      };
      "XF86AudioLowerVolume" = {
        allow-when-locked = true;
        action.spawn = [ "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%-" ];
      };
      "XF86AudioMute" = {
        allow-when-locked = true;
        action.spawn = [ "wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle" ];
      };
      "XF86AudioMicMute" = {
        allow-when-locked = true;
        action.spawn = [ "wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle" ];
      };

      # Brightness control
      "XF86MonBrightnessUp" = {
        allow-when-locked = true;
        action.spawn = [ "brightnessctl" "set" "5%+" ];
      };
      "XF86MonBrightnessDown" = {
        allow-when-locked = true;
        action.spawn = [ "brightnessctl" "set" "5%-" ];
      };

      # Power off monitors (for idle)
      "Mod+Shift+P".action.power-off-monitors = [];
    };

    # Window rules
    window-rules = [
      # Floating utility windows
      {
        matches = [{ app-id = "^1password$"; }];
        open-floating = true;
      }
      {
        matches = [{ app-id = "^yubioath-flutter$"; }];
        open-floating = true;
      }
      {
        matches = [{ app-id = "^org\\.pulseaudio\\.pavucontrol$"; }];
        open-floating = true;
      }
      {
        matches = [{ app-id = "^pavucontrol$"; }];
        open-floating = true;
      }
      {
        matches = [{ app-id = "^\\.blueman-manager-wrapped$"; }];
        open-floating = true;
      }
      {
        matches = [{ app-id = "^blueman-manager$"; }];
        open-floating = true;
      }
      {
        matches = [{ app-id = "^nm-connection-editor$"; }];
        open-floating = true;
      }
      {
        matches = [{ app-id = "^xdg-desktop-portal-gtk$"; }];
        open-floating = true;
      }
      # XIV Launcher (game launcher)
      {
        matches = [{ app-id = "^XIVLauncher\\.Core$"; }];
        open-floating = true;
      }
      # Plexamp (music player - small floating window)
      {
        matches = [{ app-id = "^Plexamp$"; }];
        open-floating = true;
      }

      # Privacy: block out from screencasts
      {
        matches = [{ app-id = "^discord$"; }];
        block-out-from = "screencast";
      }
      {
        matches = [{ app-id = "^Signal$"; }];
        block-out-from = "screencast";
      }
      {
        matches = [{ app-id = "^1password$"; }];
        block-out-from = "screencast";
      }

      # PiP / video floating windows
      {
        matches = [{ title = "^Picture.in.[Pp]icture$"; }];
        open-floating = true;
      }
    ];

    # Animations
    animations = {
      slowdown = 1.0;

      workspace-switch = {
        enable = true;
        kind.spring = {
          damping-ratio = 1.0;
          stiffness = 1000;
          epsilon = 0.0001;
        };
      };

      horizontal-view-movement = {
        enable = true;
        kind.spring = {
          damping-ratio = 1.0;
          stiffness = 800;
          epsilon = 0.0001;
        };
      };

      window-open = {
        enable = true;
        kind.easing = {
          duration-ms = 150;
          curve = "ease-out-expo";
        };
      };

      window-close = {
        enable = true;
        kind.easing = {
          duration-ms = 150;
          curve = "ease-out-quad";
        };
      };
    };
  };
}
