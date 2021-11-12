local lsp = require('lspconfig')

lsp.hls.setup{on_attach=require('completion').on_attach}
lsp.pyright.setup{}
lsp.purescriptls.setup{}

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
        virtual_text = false,
        underline = true,
        signs = true,
    }
)

