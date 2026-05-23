# OsString Conversions Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `From`/`TryFrom` instances between `System.OsString.OsString` and witch's existing string-like types (`String`, `Text`, `LazyText`, `[OsChar]`), resolving <https://github.com/tfausak/witch/issues/155>.

**Architecture:** Add a single new dependency (`os-string ^>=2.0`) and a new "OsString" block in `Witch.Instances` that follows the surrounding style. Tests go in the existing HUnit suite, modeled on the `ShortByteString` and `Text` groups. No new modules, no new public exports beyond instances.

**Tech Stack:** Haskell 2010, cabal, `os-string` package, witch's existing `From`/`TryFrom` typeclasses, HUnit.

**Spec:** `docs/superpowers/specs/2026-05-23-osstring-conversions-design.md`

---

## File Structure

- **Modify** `witch.cabal` — add `os-string ^>=2.0` to the `common library` build-depends list.
- **Modify** `source/library/Witch/Instances.hs` — add a new `-- OsString` section with six instances, plus imports for `Control.Exception.SomeException`, `System.OsString`, and `System.OsString.OsChar`.
- **Modify** `source/test-suite/Main.hs` — add imports for `System.OsString`/`OsChar`, plus a block of HUnit groups for the new instances.
- **No new files.** No changes to `Witch.Encoding` or other public modules.

---

## Task 1: Add `os-string` dependency

**Files:**
- Modify: `witch.cabal` (the `common library` block, around the `text` line)

- [ ] **Step 1: Add the dependency**

Open `witch.cabal`. In the `common library` block, find:

```
  build-depends:
    bytestring ^>=0.12.0,
    containers ^>=0.7 || ^>=0.8,
    tagged ^>=0.8.8,
    text ^>=2.1,
    time ^>=1.12.2 || ^>=1.14 || ^>=1.15,
```

Add `os-string ^>=2.0,` in alphabetical position (between `containers` and `tagged`):

```
  build-depends:
    bytestring ^>=0.12.0,
    containers ^>=0.7 || ^>=0.8,
    os-string ^>=2.0,
    tagged ^>=0.8.8,
    text ^>=2.1,
    time ^>=1.12.2 || ^>=1.14 || ^>=1.15,
```

- [ ] **Step 2: Verify the project still builds**

Run: `cabal build all`
Expected: Successful build. If cabal can't find `os-string`, run `cabal update` first.

- [ ] **Step 3: Commit**

```bash
git add witch.cabal
git commit -m "Add os-string dependency"
```

---

## Task 2: Add `From [OsChar] OsString` and `From OsString [OsChar]`

**Files:**
- Modify: `source/library/Witch/Instances.hs`
- Test: `source/test-suite/Main.hs`

- [ ] **Step 1: Add test stubs first**

Open `source/test-suite/Main.hs`. Add these imports near the existing string imports (after the `import qualified Data.ByteString.Short as ShortByteString` line):

```haskell
import qualified System.OsString as OsString
import qualified System.OsString.Internal.Types as OsChar
```

Wait — check what `os-string` actually exports for `OsChar`. The user-facing module is `System.OsString` which exports `OsChar` and `unsafeFromChar` (constructor for `OsChar`). Use:

```haskell
import qualified System.OsString as OsString
```

…and reference `OsString.OsChar` directly (it's re-exported from `System.OsString`).

Then locate the `From ShortByteString ByteString` block near line 1846 in `Main.hs`. Right after it, before `describe "From Text LazyText"`, add:

```haskell
    describe "From [OsChar] OsString" $ do
      let f = Witch.from @[OsString.OsChar] @OsString.OsString
      it "works" $ do
        f [] `shouldBe` OsString.pack []
        f [OsString.unsafeFromChar 'a'] `shouldBe` OsString.pack [OsString.unsafeFromChar 'a']
        f [OsString.unsafeFromChar 'a', OsString.unsafeFromChar 'b']
          `shouldBe` OsString.pack [OsString.unsafeFromChar 'a', OsString.unsafeFromChar 'b']

    describe "From OsString [OsChar]" $ do
      let f = Witch.from @OsString.OsString @[OsString.OsChar]
      it "works" $ do
        f (OsString.pack []) `shouldBe` []
        f (OsString.pack [OsString.unsafeFromChar 'a']) `shouldBe` [OsString.unsafeFromChar 'a']
        f (OsString.pack [OsString.unsafeFromChar 'a', OsString.unsafeFromChar 'b'])
          `shouldBe` [OsString.unsafeFromChar 'a', OsString.unsafeFromChar 'b']
```

- [ ] **Step 2: Run tests to confirm they fail to compile**

Run: `cabal test witch-test-suite`
Expected: Compile error — no `From [OsChar] OsString` instance. (Or it may be "Variable not in scope: Witch.from @[OsString.OsChar] @OsString.OsString" — that's fine, same root cause.)

- [ ] **Step 3: Add the instances and required imports**

Open `source/library/Witch/Instances.hs`. Near the top imports, add (alphabetically, after the `Data.ByteString.Short` line):

```haskell
import qualified System.OsString as OsString
```

Locate the `-- ShortByteString` block ending around line 1165, just before `-- Text`. Insert a new block:

```haskell
-- OsString

-- | Uses 'OsString.pack'.
instance From.From [OsString.OsChar] OsString.OsString where
  from = OsString.pack

-- | Uses 'OsString.unpack'.
instance From.From OsString.OsString [OsString.OsChar] where
  from = OsString.unpack

```

(Note the trailing blank line so `-- Text` keeps its spacing.)

- [ ] **Step 4: Run tests**

Run: `cabal test witch-test-suite`
Expected: All tests pass, including the two new `describe` blocks.

- [ ] **Step 5: Commit**

```bash
git add source/library/Witch/Instances.hs source/test-suite/Main.hs
git commit -m "Add From instances between [OsChar] and OsString"
```

---

## Task 3: Add `TryFrom String OsString` and `TryFrom OsString String`

**Files:**
- Modify: `source/library/Witch/Instances.hs`
- Test: `source/test-suite/Main.hs`

- [ ] **Step 1: Add test stubs**

Open `source/test-suite/Main.hs`. Find the `describe "TryFrom"` block (it's the second top-level block under `describe "Witch"`, around line 55, and it contains many sub-blocks). Locate where the existing UTF-8 / UTF-16 string TryFrom groups are tested (search for `describe "TryFrom .* Utf8`). Anywhere in that section is fine — append at the end of the `TryFrom` block, before the next outer `describe`.

Easiest landmark: search for the last `describe` whose label starts with `"TryFrom "` and add after it. Add:

```haskell
    describe "TryFrom String OsString" $ do
      let f = hush . Witch.tryFrom @String @OsString.OsString
      it "works" $ do
        f "" `shouldBe` Just (OsString.pack [])
        f "hello" `shouldBe` Just (Witch.unsafeFrom @String "hello")

    describe "TryFrom OsString String" $ do
      let f = hush . Witch.tryFrom @OsString.OsString @String
      it "works" $ do
        f (Witch.unsafeFrom @String "") `shouldBe` Just ""
        f (Witch.unsafeFrom @String "hello") `shouldBe` Just "hello"
```

Note: `Witch.unsafeFrom @String "hello"` constructs the expected `OsString` via the same `TryFrom String OsString` instance we're testing. This is fine — the test's value is round-trip correctness, not bootstrapping a known-good `OsString` from outside the library.

- [ ] **Step 2: Run tests, expect compile failure**

Run: `cabal test witch-test-suite`
Expected: Compile error — no `TryFrom String OsString` or `TryFrom OsString String` instance.

- [ ] **Step 3: Add the instances**

Open `source/library/Witch/Instances.hs`. Near the top imports add (after the existing `Control.Exception` import, or add it if missing — check first):

```haskell
import qualified Control.Exception as Exception
```

(If `Control.Exception` is already imported with a different alias or unqualified, reuse it instead of adding a duplicate.)

In the `-- OsString` block from Task 2, append after the two `From` instances:

```haskell
-- | Uses 'OsString.encodeUtf'.
instance TryFrom.TryFrom String OsString.OsString where
  tryFrom =
    Utility.eitherTryFrom $ \s ->
      OsString.encodeUtf s :: Either Exception.SomeException OsString.OsString

-- | Uses 'OsString.decodeUtf'.
instance TryFrom.TryFrom OsString.OsString String where
  tryFrom =
    Utility.eitherTryFrom $ \os ->
      OsString.decodeUtf os :: Either Exception.SomeException String
```

Why the explicit type signature: `encodeUtf`/`decodeUtf` are `MonadThrow m => ...`. We need to pin `m` to `Either SomeException` so that `Utility.eitherTryFrom` accepts the result. `Either SomeException` has a `MonadThrow` instance that stores the thrown exception in `Left`.

- [ ] **Step 4: Run tests**

Run: `cabal test witch-test-suite`
Expected: All tests pass.

- [ ] **Step 5: Commit**

```bash
git add source/library/Witch/Instances.hs source/test-suite/Main.hs
git commit -m "Add TryFrom instances between String and OsString"
```

---

## Task 4: Add `TryFrom Text OsString` and `TryFrom OsString Text`

**Files:**
- Modify: `source/library/Witch/Instances.hs`
- Test: `source/test-suite/Main.hs`

- [ ] **Step 1: Add test stubs**

In `source/test-suite/Main.hs`, immediately after the two `String ↔ OsString` describes from Task 3, add:

```haskell
    describe "TryFrom Text OsString" $ do
      let f = hush . Witch.tryFrom @Text.Text @OsString.OsString
      it "works" $ do
        f (Text.pack "") `shouldBe` Just (Witch.unsafeFrom @String "")
        f (Text.pack "hello") `shouldBe` Just (Witch.unsafeFrom @String "hello")

    describe "TryFrom OsString Text" $ do
      let f = hush . Witch.tryFrom @OsString.OsString @Text.Text
      it "works" $ do
        f (Witch.unsafeFrom @String "") `shouldBe` Just (Text.pack "")
        f (Witch.unsafeFrom @String "hello") `shouldBe` Just (Text.pack "hello")
```

- [ ] **Step 2: Run tests, expect compile failure**

Run: `cabal test witch-test-suite`
Expected: Compile error — no `TryFrom Text OsString` / `TryFrom OsString Text` instance.

- [ ] **Step 3: Add the instances**

In the `-- OsString` block in `source/library/Witch/Instances.hs`, after the two `String ↔ OsString` `TryFrom` instances, append:

```haskell
-- | Converts via 'String'.
instance TryFrom.TryFrom Text.Text OsString.OsString where
  tryFrom =
    Utility.eitherTryFrom $ \t ->
      OsString.encodeUtf (Text.unpack t) :: Either Exception.SomeException OsString.OsString

-- | Converts via 'String'.
instance TryFrom.TryFrom OsString.OsString Text.Text where
  tryFrom =
    Utility.eitherTryFrom $ \os ->
      fmap Text.pack (OsString.decodeUtf os :: Either Exception.SomeException String)
```

- [ ] **Step 4: Run tests**

Run: `cabal test witch-test-suite`
Expected: All tests pass.

- [ ] **Step 5: Commit**

```bash
git add source/library/Witch/Instances.hs source/test-suite/Main.hs
git commit -m "Add TryFrom instances between Text and OsString"
```

---

## Task 5: Add `TryFrom LazyText OsString` and `TryFrom OsString LazyText`

**Files:**
- Modify: `source/library/Witch/Instances.hs`
- Test: `source/test-suite/Main.hs`

- [ ] **Step 1: Add test stubs**

In `source/test-suite/Main.hs`, immediately after the `Text ↔ OsString` describes from Task 4, add:

```haskell
    describe "TryFrom LazyText OsString" $ do
      let f = hush . Witch.tryFrom @LazyText.Text @OsString.OsString
      it "works" $ do
        f (LazyText.pack "") `shouldBe` Just (Witch.unsafeFrom @String "")
        f (LazyText.pack "hello") `shouldBe` Just (Witch.unsafeFrom @String "hello")

    describe "TryFrom OsString LazyText" $ do
      let f = hush . Witch.tryFrom @OsString.OsString @LazyText.Text
      it "works" $ do
        f (Witch.unsafeFrom @String "") `shouldBe` Just (LazyText.pack "")
        f (Witch.unsafeFrom @String "hello") `shouldBe` Just (LazyText.pack "hello")
```

- [ ] **Step 2: Run tests, expect compile failure**

Run: `cabal test witch-test-suite`
Expected: Compile error — no `TryFrom LazyText OsString` / `TryFrom OsString LazyText` instance.

- [ ] **Step 3: Add the instances**

In the `-- OsString` block in `source/library/Witch/Instances.hs`, after the two `Text ↔ OsString` instances, append:

```haskell
-- | Converts via 'String'.
instance TryFrom.TryFrom LazyText.Text OsString.OsString where
  tryFrom =
    Utility.eitherTryFrom $ \t ->
      OsString.encodeUtf (LazyText.unpack t) :: Either Exception.SomeException OsString.OsString

-- | Converts via 'String'.
instance TryFrom.TryFrom OsString.OsString LazyText.Text where
  tryFrom =
    Utility.eitherTryFrom $ \os ->
      fmap LazyText.pack (OsString.decodeUtf os :: Either Exception.SomeException String)
```

- [ ] **Step 4: Run tests**

Run: `cabal test witch-test-suite`
Expected: All tests pass.

- [ ] **Step 5: Commit**

```bash
git add source/library/Witch/Instances.hs source/test-suite/Main.hs
git commit -m "Add TryFrom instances between LazyText and OsString"
```

---

## Task 6: Full verification with pedantic flag

**Files:** none modified.

- [ ] **Step 1: Build with -Werror via the pedantic flag**

Run: `cabal build all --flag pedantic`
Expected: Clean build, no warnings. The witch.cabal `pedantic` flag turns `-Weverything` warnings into errors, which is the strictest possible check.

If a warning fires (e.g., unused import of `Control.Exception` if Task 3 added it unnecessarily, or a missing-signatures warning on a `let`), fix it inline and re-run.

- [ ] **Step 2: Run both test suites**

Run: `cabal test all`
Expected: Both `witch-test-suite` and `witch-hedgehog` pass. (The hedgehog suite covers numeric types and shouldn't be affected, but confirm it still passes.)

- [ ] **Step 3: Sanity check — `OsPath` works transparently**

Open a `ghci` session: `cabal repl`. At the prompt:

```
> :set -XTypeApplications
> import qualified System.OsPath as OsPath
> import qualified Witch
> Witch.tryFrom @String @OsPath.OsPath "hello"
```

Expected: `Right "hello"` (or platform-specific show output for an `OsPath`/`OsString`). This confirms `OsPath` picks up the instance for free because it's a type alias for `OsString`.

If `System.OsPath` isn't importable (i.e., `filepath` isn't a transitive dep available in the repl), skip this step — the spec only requires `os-string` and `OsPath` reuse is a documented but untested consequence.

- [ ] **Step 4: Commit only if step 1 required fixes**

If no warnings fired in Step 1, no further commit is needed. If you fixed warnings:

```bash
git add -u
git commit -m "Fix pedantic warnings in OsString instances"
```

---

## Self-Review Notes

- **Spec coverage:** All six instances from the spec table are implemented across Tasks 2–5; dependency add is Task 1; pedantic build check is Task 6.
- **Mixed total/fallible bridges:** Tasks 4 and 5 inline the composition (`encodeUtf . unpack` and `fmap pack . decodeUtf`) inside `eitherTryFrom`. This matches the existing pattern at `Instances.hs:1384`.
- **Import deduplication:** Task 3 reminds the engineer to check whether `Control.Exception` is already imported before adding a new line. If it is, reuse the existing alias.
- **No version/changelog work** per user direction in the brainstorming session.
