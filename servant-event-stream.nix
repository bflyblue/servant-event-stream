let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { inherit config; };
  compilerVersion = "ghc8104";
  compilerSet = pkgs.haskell.packages."${compilerVersion}";
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = with pkgs.haskell.lib; self: super: {
          servant-event-stream = super.callCabal2nix "servant-event-stream" (gitIgnore [./.gitignore] ./.) {};
          servant-js = markUnbroken (doJailbreak super.servant-js);
        };
      };
    };
  };

in {
  inherit pkgs;
  servant-event-stream = compilerSet.servant-event-stream;
  shell = compilerSet.shellFor {
    packages = p: [ p.servant-event-stream ];
    buildInputs = with pkgs; [
      compilerSet.cabal-install
      compilerSet.haskell-language-server
    ];
  };
}
