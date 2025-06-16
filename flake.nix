{
  description = "some config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mkAlias = {
      url = "github:reckenrode/mkAlias";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL";
    };
    mac-app-util.url = "github:hraban/mac-app-util";
    dagger.url = "github:dagger/nix";
    dagger.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay/bb1a28197681dc640b89a9a9bec75cdcd7e8d6ec";
    nix-darwin.url = "github:nix-darwin/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
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
    emacs-overlay,
    nix-darwin,
    ...
  }:
  let
  linux-pkgs = import nixpkgs {
    system = "x86_64-linux";
    config.allowUnfree = true;
  };

  darwin-pkgs = import nixpkgs {
    system = "aarch64-darwin";
    config.allowUnfree = true;
    overlays = [
      emacs-overlay.overlays.emacs
    ];
  };
  in
  {
    darwinConfigurations = {
      jormungand = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = { inherit inputs; };
        modules = [
          mac-app-util.darwinModules.default
          ./systems/jormungand
          home-manager.darwinModules.home-manager
          {
            home-manager.useUserPackages = true;
            home-manager.users.femi = import ./home/home-darwin-personal.nix;
            home-manager.sharedModules = [
              mac-app-util.homeManagerModules.default
            ];
            home-manager.extraSpecialArgs =
            let
            username = "femi";
            in
            {
              inherit
              username
              self
              home-manager
              ;
              pkgs = darwin-pkgs;
            };
          }
        ];
      };
      proletariat = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        specialArgs = { inherit inputs; };
        modules = [
          mac-app-util.darwinModules.default
          ./systems/proletariat
          home-manager.darwinModules.home-manager
          {
            home-manager.useUserPackages = true;
            home-manager.users.fagbabiaka = import ./home/home-darwin.nix;
            home-manager.sharedModules = [
              mac-app-util.homeManagerModules.default
            ];
            home-manager.extraSpecialArgs =
            let
            username = "fagbabiaka";
            in
            {
              inherit
              username
              self
              home-manager
              inputs
              ;
              pkgs = darwin-pkgs;
            };
          }
        ];
      };
    };
    nixosConfigurations = {
      cassiopeia = nixpkgs.lib.nixosSystem {
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
              pkgs = linux-pkgs;
            };
          }
        ];
      };
      tachibana = nixpkgs.lib.nixosSystem {
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
              pkgs = linux-pkgs;
            };
          }
        ];
      };
      laincomp = nixpkgs.lib.nixosSystem {
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
              pkgs = linux-pkgs;
            };
          }
        ];
      };
      brain = nixpkgs.lib.nixosSystem {
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
              pkgs = linux-pkgs;
            };
          }
        ];
      };
    };

    homeConfigurations = {
      worklaptop = home-manager.lib.homeManagerConfiguration {
        pkgs = darwin-pkgs;
        modules = [
          ./home/home-darwin.nix
          mac-app-util.homeManagerModules.default
        ];
        extraSpecialArgs =
        let
        username = "fagbabiaka";
        homeDirectory = "/Users/${username}";
        in
        {
          inherit
          username
          homeDirectory
          inputs
          dagger
          ;
          pkgs = darwin-pkgs;
          system = "aarch64-darwin";
        };
      };
    };

    formatter."x86_64-linux" = linux-pkgs.nixfmt-rfc-style;
    formatter."x86_64-darwin" = darwin-pkgs.nixfmt-rfc-style;
    formatter."aarch64-darwin" = darwin-pkgs.nixfmt-rfc-style;

    devShells."x86_64-linux".default = with linux-pkgs; mkShell { packages = [ nixfmt ]; };

    devShells."aarch64-darwin".default = with darwin-pkgs; mkShell { packages = [ nixfmt ]; };
  };
}
