# OsString Conversions

Issue: <https://github.com/tfausak/witch/issues/155>

## Goal

Add `From`/`TryFrom` instances for `System.OsString.OsString` so witch users
can convert between `OsString` and witch's existing string-like types
(`String`, `Text`, `LazyText`, `[OsChar]`) using the same pattern as the
existing `Text`/`ByteString`/`ShortByteString` instances.

`OsString` is platform-dependent: on Windows it carries UTF-16LE, on POSIX it
carries raw bytes. Conversions to/from `String` therefore go through
encoding/decoding and can fail, so they must be `TryFrom`.

`OsPath` is a type alias for `OsString` (`System.OsPath.OsPath`), so it picks
up every `OsString` instance automatically — no extra work needed.

## Dependency

Add `os-string ^>=2.0` to the `common library` build-depends. We deliberately
do **not** depend on `filepath`; `os-string` is the smaller, focused package
that owns the type. `OsPath` from `filepath` is just `OsString`, so users who
want `OsPath` conversions get them for free.

## Instances

All instances live in `Witch.Instances` alongside the other string-like
instances (after the `ShortByteString` block, before the `Text` block).

**Total conversions (`From`):**

| From          | To            | Implementation        |
| ------------- | ------------- | --------------------- |
| `[OsChar]`    | `OsString`    | `OsString.pack`       |
| `OsString`    | `[OsChar]`    | `OsString.unpack`     |

**Fallible conversions (`TryFrom`):**

| From          | To            | Implementation                                             |
| ------------- | ------------- | ---------------------------------------------------------- |
| `String`      | `OsString`    | `eitherTryFrom $ OsString.encodeUtf` (specialize `m` to `Either SomeException`) |
| `OsString`    | `String`      | `eitherTryFrom $ OsString.decodeUtf` (same)                |
| `Text`        | `OsString`    | `eitherTryFrom $ OsString.encodeUtf . From.from @String`   |
| `OsString`    | `Text`        | `eitherTryFrom (decodeUtf)` then `From.from @String` the success |
| `LazyText`    | `OsString`    | same shape as `Text` via `String`                          |
| `OsString`    | `LazyText`    | same shape, post-converting `String → LazyText`            |

The `Text`/`LazyText` bridges must compose a total step with a fallible step,
which `Utility.via` (totally-total) and `Utility.tryVia` (fallibly-fallible)
don't directly support. The existing code pattern at `Instances.hs:1384` —
`Utility.eitherTryFrom $ <fallible> . From.from` — handles the `Text → OsString`
direction. The `OsString → Text` direction is symmetric: run `decodeUtf` via
`eitherTryFrom`, then `fmap From.from` over the result. Concrete code shape
will be locked in by the implementation plan.

`OsString.encodeUtf` and `decodeUtf` are `MonadThrow m => ...`, so
specializing `m ~ Either SomeException` gives a pure `Either` directly usable
with `eitherTryFrom`. No `tryEvaluate` wrapping needed.

Documentation Haddocks should match the surrounding style (one-line `-- |
Uses 'foo'.` or `-- | Converts via 'Bar'.`).

## Failure modes

`OsString.encodeUtf` and `decodeUtf` use `MonadThrow`; we surface the thrown
exception via `Utility.eitherTryFrom` and `tryEvaluate` in the existing style.
Concretely:

- `String → OsString` can fail on Windows when the string contains lone
  surrogates (the UTF-16 encoder rejects them).
- `OsString → String` can fail on POSIX when bytes are not valid UTF-8, and
  on Windows when surrogate pairs are malformed.

## Tests

`source/test-suite/Main.hs` already groups tests by source/target type. Add
parallel test groups for the new instances. Because the failure modes of
`encodeUtf`/`decodeUtf` are platform-dependent, the test suite sticks to
cross-platform-safe assertions:

- `From [OsChar] OsString` and `From OsString [OsChar]` — round-trip on a
  non-empty list of `OsChar`s.
- `TryFrom String OsString` — success on an ASCII string.
- `TryFrom OsString String` — round-trip of a known-good `OsString` produced
  from ASCII via `unsafeEncodeUtf` (or via the `From String OsString` `TryFrom`
  in a setup step).
- `TryFrom Text OsString` / `TryFrom OsString Text` — round-trip on an ASCII
  payload.
- `TryFrom LazyText OsString` / `TryFrom OsString LazyText` — same.

No hedgehog property tests in this PR; the existing `source/hedgehog/Main.hs`
covers numeric types and we don't need to expand its scope here.

## Out of scope

- `ShortByteString ↔ OsString` — `OsString` is internally `ShortByteString`,
  but the semantics differ per platform. Exposing this would leak
  implementation details and let users build `OsString` values that are
  invalid on the host platform.
- `ByteString ↔ OsString` via `fromBytes` — same reasoning as above.
- `PosixString` / `WindowsString` — platform-specific newtypes; out of scope
  per scoping decision.
- `OsPath` — already covered transparently (it's a type alias).

