# Changelog

All notable changes between releases will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added

* The LSP implementation now supports
  [incremental LSP updates](https://microsoft.github.io/language-server-protocol/specification#textDocument_synchronization)
  which means that changes in a file don't cause the full file to be transmitted to the LSP from
  your editor, but only a changeset. This should notably increase performance and reduce
  the memory footprint (from [@Ma27](https://github.com/Ma27/) & [@aaronjanse](https://github.com/aaronjanse)).

* Support for auto-completing [Nix builtins](https://nixos.org/manual/nix/unstable/expressions/builtins.html) was added.

  With Nix 2.3 a hard-coded list of all builtins is provided, with Nix 2.4 also documentation
  and arguments are provided via `nix __dump-builtins` (from [@Ma27](https://github.com/Ma27/)).

* The `document link`-operation of LSP now properly detects directories containing `/default.nix`.
  Also, the procedure was optimized (by [@aaronjanse](https://github.com/aaronjanse)).

### Changed

* `nixpkgs-fmt` was updated to [1.2.0](https://github.com/nix-community/nixpkgs-fmt/releases/tag/v1.2.0). This means that formatting Nix code via LSP produces the same result as using `nixpkgs-fmt` on the CLI (by [@jD91mzm2](https://github.com/jD91mzm2)).

* The LSP now responds with `MethodNotFound` to unsupported operations (by [@wiktorkuchta](https://github.com/wiktorkuchta)).

* `inherit`-expressions and `var@{ a, b, … }` have proper autocompletion now (only for variables
  from within the same file, currently) (by [@Ma27](https://github.com/Ma27/)).

## [v0.1.0] - 2020-01-12

* Initial release.

[Unreleased]: https://github.com/nix-community/rnix-lsp/compare/v0.1.0...master
[v0.1.0]: https://github.com/nix-community/rnix-lsp/compare/b3586e567c1e558988416676680833294699aeaa...v0.1.0
