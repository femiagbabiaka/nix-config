# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{
  pkgs,
  ...
}:
let
  checkLid = pkgs.writeShellScriptBin "check-lid" ''
    LID_STATE=$(grep -o "open" /proc/acpi/button/lid/LID/state || true)
    if [ "$LID_STATE" = "open" ]; then
      exit 0
    else
      exit 1
    fi
  '';
in
{
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  nix.gc.automatic = true;
  fileSystems."/".options = [
    "noatime"
    "nodiratime"
    "discard"
  ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nixpkgs.config.allowUnfree = true;

  networking.hostName = "giljotin"; # Define your hostname.

  # Configure network connections interactively with nmcli or nmtui.
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.powersave = false;
  networking.wireless.iwd.enable = true;
  networking.networkmanager.wifi.backend = "iwd";

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true; # use xkb.options in tty.
  };

  services = {
	  desktopManager.plasma6.enable = true;

	  displayManager.sddm.enable = true;

	  displayManager.sddm.wayland.enable = true;
  };


  services.fprintd.enable = true;
  services.fprintd.tod.enable = true;
  services.fprintd.tod.driver = pkgs.libfprint-2-tod1-goodix; # Goodix driver module
  security.pam.services = {
    # Apply to sudo
    sudo.text = pkgs.lib.mkDefault (
      pkgs.lib.mkBefore ''
        auth [success=ignore default=1] pam_exec.so quiet ${checkLid}/bin/check-lid
      ''
    );

    # Optional: Apply to login/GDM if you want the same behavior there
    login.text = pkgs.lib.mkDefault (
      pkgs.lib.mkBefore ''
        auth [success=ignore default=1] pam_exec.so quiet ${checkLid}/bin/check-lid
      ''
    );
  };

  # Configure keymap in X11
  services.xserver.xkb.layout = "us";
  services.xserver.xkb.options = "ctrl:nocaps";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput = {
    enable = true;
    touchpad = {
      tapping = true;
      clickMethod = "clickfinger";
      naturalScrolling = true;
    };
  };
  programs.fish.enable = true;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.femi = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups = [ "wheel" "input" ]; # Enable ‘sudo’ for the user.
    packages = with pkgs; [
      tree
    ];
  };

  services.fwupd = {
    enable = true;
    extraTrustedKeys = [];
    daemonSettings = {
      DisabledPlugins = [ "drm_dp_aux" ];
    };
  };

  # programs.firefox.enable = true;

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    git
        # KDE
    kdePackages.discover # Optional: Install if you use Flatpak or fwupd firmware update sevice
    kdePackages.kcalc # Calculator
    kdePackages.kcharselect # Tool to select and copy special characters from all installed fonts
    kdePackages.kclock # Clock app
    kdePackages.kcolorchooser # A small utility to select a color
    kdePackages.kolourpaint # Easy-to-use paint program
    kdePackages.ksystemlog # KDE SystemLog Application
    kdePackages.sddm-kcm # Configuration module for SDDM
    kdiff3 # Compares and merges 2 or 3 files or directories
    kdePackages.isoimagewriter # Optional: Program to write hybrid ISO files onto USB disks
    kdePackages.partitionmanager # Optional: Manage the disk devices, partitions and file systems on your computer
    # Non-KDE graphical packages
    hardinfo2 # System information and benchmarks for Linux systems
    vlc # Cross-platform media player and streaming server
    wayland-utils # Wayland utilities
    wl-clipboard # Command-line copy/paste utilities for Wayland
    kdePackages.ksshaskpass
  ];

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
  };

  services.pcscd.enable = true;
  services.tailscale.enable = true;
  services.udev.packages = [ pkgs.yubikey-personalization ];
  programs.gnupg.agent = {
    enable = false;
    enableSSHSupport = false;
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "25.11"; # Did you read the comment?

}
