let
  shellWith = (import ./. {}).servant-event-stream-shell-with;
  devpkgs = p: [ p.hlint ];
in
  shellWith devpkgs
