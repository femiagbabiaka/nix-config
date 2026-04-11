# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    ./lemonade.nix
    ./mcpo.nix
  ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "cerebro"; # Define your hostname.

  # Configure network connections interactively with nmcli or nmtui.
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/Chicago";

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  nixpkgs.config.allowUnfree = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.femi = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    packages = with pkgs; [
      fish
      zoxide
      delta
      bat
      fzf
      fastfetch
      htop
    ];
  };

  users.users.tramp = {
    isNormalUser = true;
    extraGroups = ["wheel"];
    shell = pkgs.bash;
    packages = with pkgs; [
      git
    ];
  };

  environment.systemPackages = with pkgs; [
    inputs.llm-agents.packages.x86_64-linux.claude-code
    cifs-utils
    delve
    git
    go
    gopls
    gotools
    libdrm
    racket
    ripgrep
    rocmPackages.amdsmi
    rustup
    helix # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  ];

  services.prowlarr.enable = true;

  # Bind Prowlarr to all interfaces (tailnet-only host)
  systemd.services.prowlarr.preStart = ''
    CONFIG="/var/lib/prowlarr/.config/Prowlarr/config.xml"
    if [ -f "$CONFIG" ]; then
      ${pkgs.gnused}/bin/sed -i 's|<BindAddress>127\.0\.0\.1</BindAddress>|<BindAddress>0.0.0.0</BindAddress>|' "$CONFIG"
    fi
  '';

  services.plex = {
    enable = true;
    dataDir = "/var/lib/plex";
    openFirewall = true;
  };

  services.open-webui = {
    enable = true;
    host = "0.0.0.0";
    port = 8080;
    environment = {
      OLLAMA_API_BASE_URL = "";
      OPENAI_API_BASE_URLS = "http://127.0.0.1:13305/api/v1";
      OPENAI_API_KEYS = "lemonade";
      WEBUI_AUTH = "false";
      ENABLE_OPENAI_API = "true";
      SCARF_NO_ANALYTICS = "true";
      DO_NOT_TRACK = "true";
      ANONYMIZED_TELEMETRY = "false";
    };
  };

  programs.fish.enable = true;

  # List services that you want to enable:
  # Enable common container config files in /etc/containers
  virtualisation.containers.enable = true;
  virtualisation = {
    podman = {
      enable = true;

      # Create a `docker` alias for podman, to use it as a drop-in replacement
      dockerCompat = true;

      # Required for containers under podman-compose to be able to talk to each other.
      defaultNetwork.settings.dns_enabled = true;
    };
  };

  services.tailscale = {
    enable = true;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  programs.mosh.enable = true;



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
  system.stateVersion = "26.05"; # Did you read the comment?

}
