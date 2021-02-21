" Use the following command to inject rnix-lsp as your nix language server in
" vim. Only works with LanguageClient-neovim, currently.
"
" :source %
let g:LanguageClient_serverCommands.nix = [expand('%:p:h') . '/target/debug/rnix-lsp']
