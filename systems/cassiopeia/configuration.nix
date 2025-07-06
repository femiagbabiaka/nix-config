# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).
{ pkgs, ... }:
{
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  nix.gc.automatic = true;
  nix.extraOptions = ''
    binary-caches-parallel-connections = 3
    connect-timeout = 5
  '';

  fileSystems."/".options = [
    "noatime"
    "nodiratime"
    "discard"
  ];

  nixpkgs.config.allowUnfree = true;

  networking.hostName = "cassiopeia"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true; # use xkbOptions in tty.
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "ctrl:nocaps";
    desktopManager = {
      xterm.enable = false;
    };

    xrandrHeads = [
      {
        output = "DP-2";
        primary = true;
      }
      {
        output = "DP-3";
        monitorConfig = ''
          Option "Rotate" "left"
          Option "LeftOf" "DP-2"
        '';
      }
    ];

    displayManager = {
      defaultSession = "none+i3";
    };

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        dmenu
        i3status
        i3lock
        i3blocks
      ];
    };
  };

  programs.fish.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.femi = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "video"
    ]; # Enable ‘sudo’ for the user.
    initialPassword = "test123";
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDuaV4yn3JSy53bFUrKhxhUnkEB7XysquwmyI7196jzTry6zUlvufAchkXGmtYQN+O/HyTWwV8CqjXlMCIqlQPd5Zt6Dg8GgIn1hAYIZQx6XoczTBmtZTs9aqV9Sw39XunUtLGILF6OYpF2Wn4k7wlHCvxO9tHjHS0p4+g1ZW7nCd8PzJ9JBeaa5Hj8UR0WrzZKm/HQT4OVnVzSuMPbAj8y8sYr6GZHZ7bQy2GOEtC8PKh2xRnj8nhNXZkPCxr4Vgso3Fj4ZHocs5z7QP8R5eoLOnJ3DHlWwliHQtaCIJx9EorISDffYyNfaQdE0w+uYjGsWHbyAlFlEuoA7brne4w7+3HHp7/BxnlO+CEsRM5tw3WXICRB4KsU1giAinZn2xiGJr5+8HVuDyRj9r8fphlqvoIlW9HWE7woTlyNzJ8Q7fWy9crLuJ9DwCZsN9AIYqJ8vp1/Fv4bF/zbB6V4Wc+uVB/rfEKEfy4xdvrY4YHuSLcMv+nMMPFCvOGsi+YpAVn+xhKmUTdDiGkDOWR62YSO/wYrBEvMel047vn66G23LFmRLthi3pWoioHqwvPJ186uDy0Ybjv7C1x/xymqt7yJlu0LCOTQVTiSX16QTjvqWOttyuMQRAvOSHgQZGNj9saD07t9zMwCxZ8c5eaGNed79Na95OROVq8am3lcnPYc2w== cardno:24_026_415"
    ];
    packages = with pkgs; [
      signal-desktop
      git
      _1password-gui-beta
      networkmanagerapplet
      neofetch
      htop
      discord
      fish
      wget
      light
      fishPlugins.done
      fishPlugins.fzf-fish
      fishPlugins.forgit
      fishPlugins.hydro
      fzf
      nushell
      tailscale
      tailscale-systray
      zoxide
      delta
      bat
      calibre
    ];
  };

  services.udev.packages = [ pkgs.yubikey-personalization ];
  services.pcscd.enable = true;
  services.tailscale.enable = true;

  fonts.packages = with pkgs; [ (nerdfonts.override { fonts = [ "FiraCode" ]; }) ];

  services.xserver.displayManager.sessionCommands = ''
     ${pkgs.xorg.xrdb}/bin/xrdb -merge <<EOF
      xterm*font: xft:Fira Code:size=14
      ! special
      *.foreground:   #cccccc
      *.background:   #222222
      *.cursorColor:  #cccccc

      ! black
      *.color0:       #222222
      *.color8:       #444444

      ! red
      *.color1:       #a74437
      *.color9:       #e52902

      ! green
      *.color2:       #809611
      *.color10:      #d6d031

      ! yellow
      *.color3:       #ee8600
      *.color11:      #877c5f

      ! blue
      *.color4:       #9aaa66
      *.color12:      #b8d190

      ! magenta
      *.color5:       #77a558
      *.color13:      #6cad2c

      ! cyan
      *.color6:       #8bd524
      *.color14:      #a7d603

      ! white
      *.color7:       #c2c2ca
      *.color15:      #bdb6a6
    EOF
  '';

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    git
    pinentry
  ];

  programs.mosh.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
    settings.KbdInteractiveAuthentication = false;
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
