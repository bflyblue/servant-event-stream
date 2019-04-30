let
  shell = (import ./servant-event-stream.nix).servant-event-stream-shell-with;
  devpkgs = p: [ p.hlint ];
in
  shell devpkgs
