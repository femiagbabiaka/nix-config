{
  description = "some config";

  inputs = {
    determinate.url = "https://flakehub.com/f/DeterminateSystems/determinate/*";
    nixpkgs.url = "git+https://codeberg.org/femi/nixpkgs-unfree";
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
    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };
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
      zen-browser,
      ...
    }:
    let
      mkSystem = import ./lib/helpers.nix { inherit inputs; };
    in
    {
      homeConfigurations = {
        femi = home-manager.lib.homeManagerConfiguration {
          pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
          extraSpecialArgs =
            let
              username = "femi";
            in
            {
              inherit
                username
                self
                home-manager
                neovim-nightly-overlay
                zen-browser
                ;
              homeDirectory = "/home/${username}";
            };
          modules = [
            (import ./home/home-linux.nix)
          ];
        };
      };

      darwinConfigurations = {
        jormungand = mkSystem.mkDarwinSystem {
          hostname = "jormungand";
          extraModules = [ determinate.darwinModules.default ];
        };
        proletariat = mkSystem.mkDarwinSystem {
          hostname = "proletariat";
          username = "fagbabiaka";
        };
      };

      nixosConfigurations = {
        cassiopeia = mkSystem.mkNixosSystem {
          hostname = "cassiopeia";
        };

        cerebro = mkSystem.mkNixosSystem {
          hostname = "cerebro";
          hardwareModules = [ nixos-hardware.nixosModules.framework-desktop-amd-ai-max-300-series ];
        };

        giljotin = mkSystem.mkNixosSystem {
          hostname = "giljotin";
          extraModules = [ determinate.nixosModules.default ];
        };

        tachibana = mkSystem.mkNixosSystem {
          hostname = "tachibana";
        };

        laincomp = mkSystem.mkNixosSystem {
          hostname = "laincomp";
          hardwareModules = [ nixos-hardware.nixosModules.dell-xps-13-9380 ];
        };

        brain = mkSystem.mkNixosSystem {
          hostname = "brain";
          username = "nixos";
          wsl = true;
        };
      };
    };
}
