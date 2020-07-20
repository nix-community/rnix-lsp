# rnix-lsp ![Crates.io](https://img.shields.io/crates/v/rnix-lsp)

A syntax-checking language server using
[rnix](https://github.com/nix-community/rnix-parser).

- [x] Syntax-checking diagnostics
- [x] Basic completion
- [x] Basic renaming
- [x] Basic goto definition
- [x] Expand selection proposal
- [x] Formatting using [nixpkgs-fmt](https://github.com/nix-community/nixpkgs-fmt)

This is beta-level quality *at best* - I didn't expect maintaining a
language server when writing rnix, the goal was that others would
flock around the parser and write a bunch of editor tooling :)

Breakages are expected. No semver compatibility before 1.x.y.

Turn on logging with `RUST_LOG=trace`, and redirect stderr to a file.

```sh
bash -c "env RUST_LOG=trace rnix-lsp 2> /tmp/rnix-lsp.log"
```

## Install

```
$ nix-build -o rnix-lsp https://github.com/nix-community/rnix-lsp/archive/master.tar.gz
$ nix-env -i ./rnix-lsp
```

## Integrate with your editor

These instructions are not fully tested - see issue #3. Please raise
an issue and/or send a PR if a config below didn't work out of the box.

### Vim/Neovim

#### [coc.nvim](https://github.com/neoclide/coc.nvim)

```vim
{
  "languageserver": {
    "nix": {
      "command": "rnix-lsp",
      "filetypes": [
        "nix"
      ]
    }
  }
}

```

#### [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim)

```vim
let g:LanguageClient_serverCommands = {
    \ 'nix': ['rnix-lsp']
\ }
```

#### [vim-lsp](https://github.com/prabirshrestha/vim-lsp)

```vim
if executable('rnix-lsp')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'rnix-lsp',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'rnix-lsp']},
        \ 'whitelist': ['nix'],
        \ })
endif
```

### Emacs

#### [lsp-mode](https://github.com/emacs-lsp/lsp-mode)

```elisp
(add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                  :major-modes '(nix-mode)
                  :server-id 'nix))
```
