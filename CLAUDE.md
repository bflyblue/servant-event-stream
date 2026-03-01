# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

servant-event-stream is a Haskell library that adds Server-Sent Events (SSE) support to the Servant web framework. It provides a `ServerSentEvents` type combinator and a `ToServerEvent` typeclass so users can stream custom event types from Servant endpoints using Servant's native `Stream` infrastructure.

Single module: `Servant.API.EventStream` in `src/Servant/API/EventStream.hs`.

## Build Commands

The project uses Nix flakes (GHC 9.12, nixos-25.11) with cabal underneath. direnv auto-activates the dev shell via `.envrc`.

```bash
nix develop              # enter dev shell (cabal, HLS, hoogle)
nix build                # full nix build
nix flake check          # CI-equivalent check
cabal build              # build library (inside dev shell)
cabal test               # run test suite (currently a placeholder)
cabal haddock            # generate documentation
cabal repl               # interactive REPL
```

## CI

`.github/workflows/test.yml` runs two jobs:
- **nix**: `nix build` and `nix flake check` on ubuntu-latest.
- **cabal**: matrix build across GHC 8.6, 8.10, 9.2, 9.8, 9.12 using `haskell-actions/setup` with cabal store caching.

## GHC Compatibility

- **Supported**: GHC 8.6 through 9.12 (base >=4.12 && <4.22).
- **Constrained by servant-server**: servant-server's own base bounds limit the effective range. GHC 9.14 (base 4.22) is blocked until servant bumps its upper bound.
- **CPP guard**: `Data.Semigroup` is conditionally imported for base < 4.11 (`<>` not in Prelude). This guard must remain if the base lower bound is ever dropped below 4.11.
- **`-Wno-unticked-promoted-constructors`**: suppressed for GHC < 9.4 via cabal conditional. The warning was removed in GHC 9.4; the promoted constructor `S.GET` in the `HasForeign` instance triggers it on older GHCs.
- **`Typeable` deriving**: removed from `ServerSentEvents` since all types auto-derive `Typeable`. GHC 9.12 warns about redundant `Typeable` deriving via `-Wderiving-typeable`.

## Architecture

The library is a single-module ~180-line library with these key pieces:

- **`ServerSentEvents a`** — phantom-typed data kind used as a Servant API combinator. `HasServer` instances (plain and `Headers`-wrapped) delegate to Servant's `StreamGet ServerEventFraming EventStream a`.
- **`ServerEvent`** — concrete SSE event record (`eventType`, `eventId`, `eventData` as lazy ByteStrings).
- **`ToServerEvent`** — typeclass converting user types to `ServerEvent`. Drives the `MimeRender EventStream` instance.
- **`encodeServerEvent`** — formats a `ServerEvent` per the MDN SSE spec (strips CR, splits on LF for multi-line data fields).
- **`ServerEventFraming`** — `FramingRender` instance that separates events with a trailing newline.
- **`EventStream`** — MIME type with `Accept` instance for `text/event-stream; charset=utf-8`.
- **`RecommendedEventSourceHeaders` / `recommendedEventSourceHeaders`** — adds `X-Accel-Buffering: no` and `Cache-Control: no-store` headers for reverse-proxy compatibility.
- **`HasForeign`** instance — enables servant-foreign code generation, prefixing function names with "stream".

Servant imports are qualified (as of v0.3.1.0) to avoid conflicts with servant >= 0.20.3.0.

The module haddock contains usage examples (`ToServerEvent` / `FromServerEvent` instances for a `Notification` type). The test suite in `tests/Spec.hs` has a matching "custom type" section that compiles and exercises these same patterns. If you change the haddock examples, update the test to match, and vice versa.

## Haskell Conventions

- Haskell2010 with `MultiParamTypeClasses` and `OverloadedStrings` as default extensions.
- `-Wall` enabled. Additional per-file pragmas: `CPP`, `DataKinds`, `TypeFamilies`, `PolyKinds`, `ScopedTypeVariables`, `TypeApplications`, `FlexibleContexts`, `FlexibleInstances`, `UndecidableInstances`, `DeriveGeneric`.
- Uses `lens` for optics in the `HasForeign` instance.

## Release Process

1. Update version in `servant-event-stream.cabal`.
2. Update `CHANGELOG.md` with new entry.
3. Update `tested-with` in cabal file if GHC versions changed.
4. Commit, tag (`vX.Y.Z.W`), and push tag.
5. `cabal sdist` then `cabal upload --publish` to Hackage.

Version bumps follow the [Haskell PVP](https://pvp.haskell.org/): dependency-only changes are patch-level; API or behaviour changes are minor or major.
