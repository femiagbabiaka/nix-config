# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).
{
  pkgs,
  ...
}: {
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.gc.automatic = true;
  nix.extraOptions = ''
    binary-caches-parallel-connections = 3
    connect-timeout = 5
  '';

  fileSystems."/".options = ["noatime" "nodiratime" "discard"];

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
    extraGroups = ["wheel" "video"]; # Enable ‘sudo’ for the user.
    initialPassword = "test123";
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCg1dleX+XEtt1M1A2GmY4OsYy5jl3JVdbcqZMYXMdcLXB8LCGavsYR35s3ZpT1fcv7i6o+Qyk/0kNtltIt/AA3Rm7Zpvtti+ScCtDJIxjAdaP00RsAmyvditxRuZUrE5hw7kpGgkfHetQym8HTTlMIYKA7ccnPBgLxNpP7ENafzuWsi5lHZEFzXISjRHvt1QlvVtcjx356K2Bl8s5ckmWHtYgWbNjUVaxmMTa7FdqmzBCCniZfs11GmI/m2WndANwkMAfwfE1cAluhReKowg8q/Kxz4TT2ozCaXUDnaQWnSL0KVsvlPrQAa4OkIfv6s1zK5l0uMq6zMlI89Ein5NnQOE+Q4xMHLBqSqWq4j4ZNrJ7mB35jmLUy0m8KgmtAMYlRd3UW0PoM0euElbrNHEghvoGuvmAOavU2BHV6mdCIFubsh0Db/Xln9EcyMYV5qX3GcHs2b6/Vynj1ENd6u0DJHMM0EssFP2S+rMOj31Pkc5++3emmjDWJFGlrYFE6IIQR9I9J5lV7i0Drauh5JWag+TMvZP5mxJq8tQbuOLIgns1oooSsHLHRRMveHLq1xfYxmcMkMOAfmjqosyGMNs6qcOdwZaSW7UHhanGjszvAN673FupGwXMEbPltV1yLaSeplRBPrEJ+DX/JzMkkvhXm52/YyMiZWj3BkYeB6R9tiw== openpgp:0x0635F2CF"
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
    ];
  };

  services.udev.packages = [pkgs.yubikey-personalization];
  services.pcscd.enable = true;
  services.tailscale.enable = true;

  fonts.packages = with pkgs; [
    (nerdfonts.override {fonts = ["FiraCode"];})
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

  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    git
    pinentry
  ];

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
    settings.X11Forwarding = true;
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [22];
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