{
  description = "some config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, nixos-hardware, home-manager, emacs-overlay }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        emacs-overlay.overlay
      ];
    };
  in
  {
    nixosConfigurations = {
      laincomp = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit inputs; };
        modules = [
          ./systems/laincomp
          ./systems/laincomp/configuration.nix
          nixos-hardware.nixosModules.dell-xps-13-9380
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.femi = import ./home/home.nix;
            home-manager.extraSpecialArgs = {
              inherit pkgs self home-manager;
              username = "femi";
              homeDirectory = "/home/${username}";
            };
          }
        ];
      };
    };
  };
}
