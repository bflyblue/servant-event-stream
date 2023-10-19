{
  description = "servant-event-stream";

  inputs = { nixpkgs.url = "github:nixos/nixpkgs"; };

  outputs = { self, nixpkgs, ... }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forallSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system:
          f (rec {
            inherit system;
            pkgs = nixpkgsFor system;
            haskellPackages = hpkgsFor pkgs;
          }));
      nixpkgsFor = system: import nixpkgs { inherit system; };
      hpkgsFor = pkgs:
        with pkgs.haskell.lib;
        pkgs.haskell.packages.ghc96.override {
          overrides = self: super: {
            servant-foreign = dontCheck super.servant-foreign;
          };
        };
    in {
      packages = forallSystems ({ system, pkgs, haskellPackages }: {
        servant-event-stream =
          haskellPackages.callCabal2nix "servant-event-stream" ./. { };
        default = self.packages.${system}.servant-event-stream;
      });
      devShells = forallSystems ({ system, pkgs, haskellPackages }: {
        servant-event-stream = haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.servant-event-stream ];
          buildInputs = with haskellPackages; [
            cabal-install
            haskell-language-server
          ];
          withHoogle = true;
        };
        default = self.devShells.${system}.servant-event-stream;
      });
    };
}
