# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, lib, ... }:
with lib;
{
  nix.enable = false;
  networking.hostName = "proletariat"; # Define your hostname.
  networking.knownNetworkServices = [
    "Wi-Fi"
    "Thunderbolt Bridge"
  ];
  system.primaryUser = "fagbabiaka";

  # Set your time zone.
  time.timeZone = "America/Chicago";

  fonts.packages = with pkgs; [ nerd-fonts.fira-code ];
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.fagbabiaka = {
    shell = pkgs.fish;
    home = "/Users/fagbabiaka";
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    helix # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  ];

  environment.shells = with pkgs; [ fish ];

  programs.fish.enable = true;

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };

  services.tailscale = {
    enable = true;
    overrideLocalDns = true;
  };

  services.yabai.enable = false;

  services.sketchybar.enable = true;

  services.aerospace = {
    enable = true;
    settings = {
      after-startup-command = [ "exec-and-forget sketchybar" ];
      exec-on-workspace-change = [
        "/bin/bash"
        "-c"
        "sketchybar --trigger aerospace_workspace_change FOCUSED=$AEROSPACE_FOCUSED_WORKSPACE"
      ];
      gaps = {
        outer.left = 10;
        outer.bottom = 10;
        outer.top = 10;
        outer.right = 10;
        inner.horizontal = 10;
        inner.vertical = 10;
      };
      "on-focused-monitor-changed" = [ "move-mouse monitor-lazy-center" ];
      mode.main.binding = {
        "alt-enter" = "exec-and-forget ${pkgs.kitty}/bin/kitty -1 --directory ~";
        "alt-j" = "focus --boundaries-action wrap-around-the-workspace left";
        "alt-k" = "focus --boundaries-action wrap-around-the-workspace down";
        "alt-l" = "focus --boundaries-action wrap-around-the-workspace up";
        "alt-semicolon" = "focus --boundaries-action wrap-around-the-workspace right";
        "alt-shift-j" = "move left";
        "alt-shift-k" = "move down";
        "alt-shift-l" = "move up";
        "alt-shift-semicolon" = "move right";

        "alt-h" = "join-with left";
        "alt-v" = "join-with up";

        "alt-f" = "fullscreen";

        "alt-s" = "layout v_accordion"; # "layout stacking" in i3
        "alt-w" = "layout h_accordion"; # "layout tabbed" in i3
        "alt-e" = "layout tiles horizontal vertical"; # "layout toggle split" in i3

        "alt-shift-space" = "layout floating tiling"; # "floating toggle" in i3
        "alt-1" = "workspace 1";
        "alt-2" = "workspace 2";
        "alt-3" = "workspace 3";
        "alt-4" = "workspace 4";
        "alt-5" = "workspace 5";
        "alt-6" = "workspace 6";
        "alt-7" = "workspace 7";
        "alt-8" = "workspace 8";
        "alt-9" = "workspace 9";
        "alt-0" = "workspace 10";

        "alt-shift-1" = "move-node-to-workspace 1";
        "alt-shift-2" = "move-node-to-workspace 2";
        "alt-shift-3" = "move-node-to-workspace 3";
        "alt-shift-4" = "move-node-to-workspace 4";
        "alt-shift-5" = "move-node-to-workspace 5";
        "alt-shift-6" = "move-node-to-workspace 6";
        "alt-shift-7" = "move-node-to-workspace 7";
        "alt-shift-8" = "move-node-to-workspace 8";
        "alt-shift-9" = "move-node-to-workspace 9";
        "alt-shift-0" = "move-node-to-workspace 10";

        "alt-shift-c" = "reload-config";

        "alt-r" = "mode resize";
      };
      mode.resize.binding = {
        "h" = "resize width -50";
        "j" = "resize height +50";
        "k" = "resize height -50";
        "l" = "resize width +50";
        "enter" = "mode main";
        "esc" = "mode main";
      };
    };
  };

  homebrew = {
    enable = true;
    casks = [
      "1password"
      "yubico-authenticator"
      "zen"
      "plexamp"
      "zed"
      "keymapp"
    ];
    masApps = {
      "Wireguard" = 1451685025;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
