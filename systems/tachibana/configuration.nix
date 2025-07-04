# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  config,
  lib,
  pkgs,
  ...
}:
{
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  nix.gc.automatic = true;
  nix.extraOptions = ''
    binary-caches-parallel-connections = 3
    connect-timeout = 5
  '';

  # Allow unfree packages.
  nixpkgs.config.allowUnfree = true;

  # Use the GRUB 2 boot loader.
  # boot.loader.grub.enable = true;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  # boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only
  boot.loader.systemd-boot.enable = true;

  networking.hostName = "tachibana"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "UTC";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # hardware.pulseaudio.enable = true;
  # OR
  # services.pipewire = {
  #   enable = true;
  #   pulse.enable = true;
  # };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.femi = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.fish;
    packages = with pkgs; [
      tree
      fish
      fishPlugins.done
      fishPlugins.fzf-fish
      fishPlugins.forgit
      fishPlugins.hydro
      zoxide
      delta
      bat
      fzf
      neofetch
      htop
    ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEtIEIUvyo/++CgRlA7izwA20E1BLXp9vXYP9BazA+pY femi@femiagbabiaka.xyz"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDuaV4yn3JSy53bFUrKhxhUnkEB7XysquwmyI7196jzTry6zUlvufAchkXGmtYQN+O/HyTWwV8CqjXlMCIqlQPd5Zt6Dg8GgIn1hAYIZQx6XoczTBmtZTs9aqV9Sw39XunUtLGILF6OYpF2Wn4k7wlHCvxO9tHjHS0p4+g1ZW7nCd8PzJ9JBeaa5Hj8UR0WrzZKm/HQT4OVnVzSuMPbAj8y8sYr6GZHZ7bQy2GOEtC8PKh2xRnj8nhNXZkPCxr4Vgso3Fj4ZHocs5z7QP8R5eoLOnJ3DHlWwliHQtaCIJx9EorISDffYyNfaQdE0w+uYjGsWHbyAlFlEuoA7brne4w7+3HHp7/BxnlO+CEsRM5tw3WXICRB4KsU1giAinZn2xiGJr5+8HVuDyRj9r8fphlqvoIlW9HWE7woTlyNzJ8Q7fWy9crLuJ9DwCZsN9AIYqJ8vp1/Fv4bF/zbB6V4Wc+uVB/rfEKEfy4xdvrY4YHuSLcMv+nMMPFCvOGsi+YpAVn+xhKmUTdDiGkDOWR62YSO/wYrBEvMel047vn66G23LFmRLthi3pWoioHqwvPJ186uDy0Ybjv7C1x/xymqt7yJlu0LCOTQVTiSX16QTjvqWOttyuMQRAvOSHgQZGNj9saD07t9zMwCxZ8c5eaGNed79Na95OROVq8am3lcnPYc2w== cardno:24_026_415"
    ];
  };
  programs.fish.enable = true;

  # programs.firefox.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    git
    helix
    wget
    ethtool
  ];

  security.sudo.wheelNeedsPassword = false;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.tailscale.enable = true;
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
    "net.ipv6.conf.all.forwarding" = 1;
  };
  services.networkd-dispatcher = {
    enable = true;
    rules = {
      "tailscale" = {
        onState = [ "routable" ];
        script = ''
          #!${pkgs.runtimeShell}
          ethtool -K enp4s0 rx-udp-gro-forwarding on rx-gro-list off
        '';
      };
    };
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

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
  system.stateVersion = "24.11"; # Did you read the comment?
}
