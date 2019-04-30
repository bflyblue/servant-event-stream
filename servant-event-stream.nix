let
  sources = import nix/sources.nix;
  nixpkgs = import sources.nixpkgs {};

  inherit (nixpkgs) pkgs;
  inherit (pkgs.haskell.lib) doJailbreak dontCheck dontHaddock;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
    };
  };

  drv = haskellPackages.callCabal2nix "servant-event-stream" ./. {};

in rec
{
  servant-event-stream = drv;
  servant-event-stream-shell = servant-event-stream-shell-with (p: []);
  servant-event-stream-shell-with = devpkgs: haskellPackages.shellFor {
    packages = p: [drv];
    buildInputs = devpkgs haskellPackages;
    withHoogle = true;
    shellHook = ''
        export HIE_HOOGLE_DATABASE="$(cat $(which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
      '';
  };
}
