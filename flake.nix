{
  description = "some config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, nixos-hardware, home-manager }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      username = "femi";
      homeDirectory = "/home/${username}";
    in
    {
      nixosConfigurations = {
        laincomp = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs; };
          modules = [
            ./systems/laincomp
            nixos-hardware.nixosModules.dell-xps-13-9380
            ./configuration.nix
          ];
        };
      };

      homeConfigurations = {
        femi = home-manager.lib.homeManagerConfiguration {

          inherit pkgs;
          modules = [
            ./home/home.nix
            {
              home = {
                inherit username homeDirectory;
                stateVersion = "23.05";
              };
            }
          ];
        };
      };
    };
}
