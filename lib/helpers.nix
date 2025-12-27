{ inputs }:
let
  inherit (inputs.nixpkgs) lib;

  defaultUser = "femi";
  defaultLinuxHome = ../home/home-linux.nix;
  defaultDarwinHome = ../home/home-darwin.nix;
  personalDarwinHome = ../home/home-darwin-personal.nix;

in
{
  mkNixosSystem = {
    hostname,
    system ? "x86_64-linux",
    username ? defaultUser,
    wsl ? false,
    hardwareModules ? [],
    extraModules ? [],
  }:
  lib.nixosSystem {
    inherit system;
    specialArgs = { inherit inputs; };
    modules = [
      ../systems/${hostname}/configuration.nix
      # Include default.nix (often hardware config) if it exists
      (if builtins.pathExists ../systems/${hostname}/default.nix then ../systems/${hostname}/default.nix else {})

      inputs.home-manager.nixosModules.home-manager
      {
        home-manager.useGlobalPkgs = true;
        home-manager.useUserPackages = true;
        home-manager.users.${username} = import defaultLinuxHome;
        home-manager.extraSpecialArgs = {
          inherit inputs username;
          homeDirectory = "/home/${username}";
          self = inputs.self;
          home-manager = inputs.home-manager;
          neovim-nightly-overlay = inputs.neovim-nightly-overlay;
          zen-browser = inputs.zen-browser;
          llm-agents = inputs.llm-agents;
          pkgs = inputs.nixpkgs.legacyPackages.${system};
        };
      }
    ]
    ++ (if wsl then [ inputs.nixos-wsl.nixosModules.wsl ] else [])
    ++ hardwareModules
    ++ extraModules;
  };

  mkDarwinSystem = {
    hostname,
    system ? "aarch64-darwin",
    username ? defaultUser,
    homeConfig ? null,
    extraModules ? [],
  }:
  inputs.nix-darwin.lib.darwinSystem {
    inherit system;
    specialArgs = { inherit inputs; };
    modules = [
      inputs.mac-app-util.darwinModules.default
      ../systems/${hostname}
      inputs.home-manager.darwinModules.home-manager
      {
        home-manager.useUserPackages = true;
        home-manager.users.${username} =
          if homeConfig != null then import homeConfig
          else if username == "femi" then import personalDarwinHome
          else import defaultDarwinHome;

        home-manager.sharedModules = [ inputs.mac-app-util.homeManagerModules.default ];
        home-manager.extraSpecialArgs = {
          inherit inputs username;
          self = inputs.self;
          home-manager = inputs.home-manager;
          dagger = inputs.dagger;
          system = system;
          neovim-nightly-overlay = inputs.neovim-nightly-overlay;
          pkgs = inputs.nixpkgs.legacyPackages.${system};
        };
      }
    ] ++ extraModules;
  };
}
