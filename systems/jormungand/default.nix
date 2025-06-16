# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  lib,
  ...
}:
with lib; {
  nix.enable = false;
  networking.hostName = "jormungand"; # Define your hostname.

  # Set your time zone.
  time.timeZone = "America/Chicago";

  fonts.packages = with pkgs; [
    nerd-fonts.fira-code
  ];
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.femi = {
    shell = pkgs.fish;
    home = "/Users/femi";
    ignoreShellProgramCheck = true;
  };
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    helix # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?
}
