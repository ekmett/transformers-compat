0.4.0.4
-------
* Each of versions 0.4.0.2–0.4.0.4 is a 0.4 build with a different set of flags configured. Building this way allows us to work around bugs in `cabal`'s backtracker. The 0.4 release notes describe the changes in this version.
  This release is configured with neither `-ftwo` nor `-fthree` (which works with `transformers-0.4` and above).

0.4.0.3
-------
* Each of versions 0.4.0.2–0.4.0.4 is a 0.4 build with a different set of flags configured. Building this way allows us to work around bugs in `cabal`'s backtracker. The 0.4 release notes describe the changes in this version.
  This release is configured with `-fthree` (which works with the `transformers-0.3` series).

0.4.0.2
-------
* Each of versions 0.4.0.2–0.4.0.4 is a 0.4 build with a different set of flags configured. Building this way allows us to work around bugs in `cabal`'s backtracker. The 0.4 release notes describe the changes in this version.
  This release is configured with `-ftwo` (which works with the `transformers-0.2` series).

0.4
---
* Added support for the missing `ExceptT` instances from `mtl`.

  This was not done lightly. While this means that by default incurring a dependency on `transformers-compat` drags in `mtl` when you are
  using an old `transformers`, it means that users do not have to orphan these instances and permits wider adoption of `ExceptT`.

  If you absolutely can't stand `mtl` and really want this package to build as valid `Haskell98`, then you can use `cabal install transformers-compat -f-mtl` to avoid incurring the dependency to get these instances. However, that is effectively an unsupported configuration.

0.3.3.4
-------
* Versions 0.3.3.2–0.3.3.4 were a successful attempt to fix build problems caused by the cabal backtracker.
* Each of these is a build with a different set of flags configured.
  This release is configured with neither `-ftwo` nor `-fthree` (which works with `transformers-0.4` and above).

0.3.3.3
-------
* Versions 0.3.3.2–0.3.3.4 were a successful attempt to fix build problems caused by the cabal backtracker.
* Each of these is a build with a different set of flags configured.
  This release is configured with `-fthree` (which works with the `transformers-0.3` series).

0.3.3.2
-------
* Versions 0.3.3.2–0.3.3.4 were a successful attempt to fix build problems caused by the cabal backtracker.
* Each of these is a build with a different set of flags configured.
  This release is configured with `-ftwo` (which works with the `transformers-0.2` series).

0.3.2
-----
* This release was a failed (or at least, only partially successful) attempt to fix build problems caused by the cabal backtracker.

0.3.1
-----
* `transformers 0.4.1` compatibility

0.3
---
* Added the instances for `Data.Functor.Classes` from `transformers 0.4`
* Switched `Control.Applicative.Backwards` and `Data.Functor.Reverse` to the split constructor/accessor style from `transformers 0.4`.

0.2
---
* Added the new types and classes from `transformers 0.4`

0.1.1.1
-------
* Wrote a better synopsis

0.1.1
-----
* Updated to trick `cabal` into building an empty `libHStransformers-compat-0.1.a` on GHC 7.6.

0.1
---
* Repository initialized by pulling the `transformers-0.2` compatibility layer out of `lens`.
