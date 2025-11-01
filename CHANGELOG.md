# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- WIP: Allow running the `make` command in the browser ([#94](https://github.com/guida-lang/compiler/issues/94)).
- Initial REPL tests for basic arithmetic evaluation.
- New `test` command ([#98](https://github.com/guida-lang/compiler/issues/98)).
- Add `elm-explorations/test` dependency as part of `init` ([#65](https://github.com/guida-lang/compiler/issues/65)).
- Extend record referred by another record’s field ([#79](https://github.com/guida-lang/compiler/issues/79)).
- Add a `CONTRIBUTING.md` file ([#103](https://github.com/guida-lang/compiler/issues/103)).
- `guida format` command ([#100](https://github.com/guida-lang/compiler/issues/100)).
- Numeric separators ([#109](https://github.com/guida-lang/compiler/issues/109)).
- Binary literals support ([#2248](https://github.com/elm/compiler/issues/2248)).
- Bool type support in WebGL shader interface ([#2120](https://github.com/elm/compiler/issues/2120)).

### Fixed

- Correct reporting of multiple errors ([#99](https://github.com/guida-lang/compiler/issues/99)).
- Replaced infinite looping `Crash.crash` with log and exit ([#120](https://github.com/guida-lang/compiler/issues/120)).

### Changed

- Refactored project structure to support both Node and Browser environments.
- Refactoring of `Task` aliases for a more unified approach across the codebase ([#108](https://github.com/guida-lang/compiler/issues/108)).

---

## [1.0.0-alpha] – 2025-03-28

### Added

- Initial stable release.
- Allow tuples with 3+ elements ([#75](https://github.com/guida-lang/compiler/issues/75)).
- Support for `GUIDA_REGISTRY` environment variable.
- New `--yes` flag for `init` command ([#80](https://github.com/guida-lang/compiler/issues/80)).
* Support modifying records via qualified names ([#78](https://github.com/guida-lang/compiler/issues/78)).
- New `--package` flag for `init` command ([#43](https://github.com/guida-lang/compiler/issues/43)).
- New `--test` flag for `install` command ([#64](https://github.com/guida-lang/compiler/issues/64)).
- `uninstall` command ([#60](https://github.com/guida-lang/compiler/issues/60)).
- Source maps support ([#63](https://github.com/guida-lang/compiler/issues/63)).
- Guida-specific syntax for underscore wildcard variables ([#59](https://github.com/guida-lang/compiler/issues/59)).
- Format command (`guida format`) ([#58](https://github.com/guida-lang/compiler/issues/58), fixes [#42](https://github.com/guida-lang/compiler/issues/42)).
- Self-hosted environment ([#9](https://github.com/guida-lang/compiler/issues/9)).
- `--optimize` flag for `build` command ([#36](https://github.com/guida-lang/compiler/issues/36)).

### Changed

- Bumped version to `1.0.0`.
- Reduced size of `guida-stuff` from ~97.7 MB to ~27.5 MB.