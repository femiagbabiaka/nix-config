  {
  description = "some config";

  inputs = {
    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/*";
    nixpkgs.url = "github:femiagbabiaka/nixpkgs-unfree";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs/nixpkgs";
    };
    mkAlias = {
      url = "github:reckenrode/mkAlias";
      inputs.nixpkgs.follows = "nixpkgs/nixpkgs";
    };
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
    };
    mac-app-util.url = "github:hraban/mac-app-util";
    dagger.url = "github:dagger/nix";
    dagger.inputs.nixpkgs.follows = "nixpkgs/nixpkgs";
    nix-darwin.url = "github:nix-darwin/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs/nixpkgs";
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      nixos-hardware,
      home-manager,
      mkAlias,
      nixos-wsl,
      mac-app-util,
      dagger,
      nix-darwin,
      determinate,
      neovim-nightly-overlay,
      ...
    }:
    {
      darwinConfigurations = {
        jormungand = nix-darwin.lib.darwinSystem rec {
          system = "aarch64-darwin";
          modules = [
            mac-app-util.darwinModules.default
            ./systems/jormungand
            home-manager.darwinModules.home-manager
            {
              home-manager.useUserPackages = true;
              home-manager.users.femi = import ./home/home-darwin-personal.nix;
              home-manager.sharedModules = [ mac-app-util.homeManagerModules.default ];
              home-manager.extraSpecialArgs =
                let
                  username = "femi";
                in
                {
                  inherit
                    username
                    self
                    home-manager
                    neovim-nightly-overlay
                    ;
                  pkgs = inputs.nixpkgs.legacyPackages.${system};
                };
            }
          ];
        };
        proletariat = nix-darwin.lib.darwinSystem rec {
          system = "aarch64-darwin";
          specialArgs = { inherit inputs; };
          modules = [
            mac-app-util.darwinModules.default
            ./systems/proletariat
            home-manager.darwinModules.home-manager
            {
              home-manager.useUserPackages = true;
              home-manager.users.fagbabiaka = import ./home/home-darwin.nix;
              home-manager.sharedModules = [ mac-app-util.homeManagerModules.default ];
              home-manager.extraSpecialArgs =
                let
                  username = "fagbabiaka";
                in
                {
                  inherit
                    username
                    self
                    home-manager
                    dagger
                    system
                    neovim-nightly-overlay
                    ;
                  pkgs = inputs.nixpkgs.legacyPackages.${system};
                };
            }
          ];
        };
      };
      nixosConfigurations = {
        cassiopeia = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            ./systems/cassiopeia
            ./systems/cassiopeia/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.femi = import ./home/home-linux.nix;
              home-manager.extraSpecialArgs =
                let
                  username = "femi";
                  homeDirectory = "/home/${username}";
                in
                {
                  inherit
                    username
                    homeDirectory
                    self
                    home-manager
                    ;
                  pkgs = inputs.nixpkgs.legacyPackages.${system};
                };
            }
          ];
        };
        cerebro = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            ./systems/cerebro
            ./systems/cerebro/configuration.nix
            nixos-hardware.nixosModules.framework-desktop-amd-ai-max-300-series
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.femi = import ./home/home-linux.nix;
              home-manager.extraSpecialArgs =
                let
                  username = "femi";
                  homeDirectory = "/home/${username}";
                in
                {
                  inherit
                    username
                    homeDirectory
                    self
                    home-manager
                    ;
                  pkgs = inputs.nixpkgs.legacyPackages.${system};
                };
            }
          ];
        };
        tachibana = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            ./systems/tachibana
            ./systems/tachibana/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.femi = import ./home/home-linux.nix;
              home-manager.extraSpecialArgs =
                let
                  username = "femi";
                  homeDirectory = "/home/${username}";
                in
                {
                  inherit
                    username
                    homeDirectory
                    self
                    home-manager
                    ;
                  pkgs = inputs.nixpkgs.legacyPackages.${system};
                };
            }
          ];
        };
        laincomp = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            ./systems/laincomp
            ./systems/laincomp/configuration.nix
            nixos-hardware.nixosModules.dell-xps-13-9380
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.femi = import ./home/home-linux.nix;
              home-manager.extraSpecialArgs =
                let
                  username = "femi";
                  homeDirectory = "/home/${username}";
                in
                {
                  inherit
                    username
                    homeDirectory
                    self
                    home-manager
                    ;
                  pkgs = inputs.nixpkgs.legacyPackages.${system};
                };
            }
          ];
        };
        brain = nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          specialArgs = { inherit inputs; };
          modules = [
            ./systems/brain/configuration.nix
            nixos-wsl.nixosModules.wsl
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.nixos = import ./home/home-linux.nix;
              home-manager.extraSpecialArgs =
                let
                  username = "nixos";
                  homeDirectory = "/home/${username}";
                in
                {
                  inherit
                    username
                    homeDirectory
                    self
                    home-manager
                    ;
                  pkgs = nixpkgs.legacyPackages.${system};
                };
            }
          ];
        };
      };
    };
}
