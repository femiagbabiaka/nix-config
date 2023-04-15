# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

with lib;

{
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.gc.automatic = true;
  nix.extraOptions = ''
    binary-caches-parallel-connections = 3
    connect-timeout = 5
  '';

  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  powerManagement.enable = true;

  # Allow unfree packages.
  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "laincomp"; # Define your hostname.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.
  hardware.bluetooth.enable = true;


  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true; # use xkbOptions in tty.
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Configure keymap in X11
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "ctrl:nocaps";

  # Configure window manager
  services.xserver.windowManager.i3 = {
    enable = true;
    extraPackages = with pkgs; [
    ];
    extraSessionCommands = ''
     ${lib.strings.optionalString config.networking.networkmanager.enable "${pkgs.networkmanagerapplet}/bin/nm-applet &"}
     ${lib.strings.optionalString config.hardware.bluetooth.enable "${pkgs.blueman}/bin/blueman-applet &"}
    '';
  };

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
  ];

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

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.femi = {
    isNormalUser = true;
    extraGroups = [ "wheel" "video" ]; # Enable ‘sudo’ for the user.
    initialPassword = "test123";
    shell = pkgs.fish;
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
      zoxide
      delta
      dmenu
      i3status
      i3lock
      i3blocks
      i3
    ];
  };
  programs.fish.enable = true;
  services.dbus.enable = true;
  services.tailscale.enable = true;
  services.printing.enable = true;
  services.tlp.enable = true;
  services.upower.enable = true;
  services.blueman.enable = true;



  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    xorg.xf86videointel
    blueman
  ];

  environment.pathsToLink = [ "/libexec" ];

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # Or disable the firewall altogether.
  #networking.firewall.enable = false;
  networking.firewall.checkReversePath = "loose";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}



