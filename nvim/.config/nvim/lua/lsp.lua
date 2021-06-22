local lsp = require('lspconfig')

lsp.hls.setup{on_attach=require('completion').on_attach}
lsp.pyright.setup{}
