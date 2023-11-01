local lsp = require('lspconfig')
local util = require("lspconfig/util")

local on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    local bufopts = { noremap=true, silent=true, buffer=bufnr }
end

local servers = { 'pyright', 'hls', 'gopls', 'tsserver', 'rust_analyzer' }
for _, server in pairs(servers) do
    lsp[server].setup{
        on_attach=on_attach,
        flags = {
          debounce_text_changes = 150,
        }
    }
end


vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
        virtual_text = true,
        underline = true,
        signs = true,
    }
)
-- local null_ls = require("null-ls")
-- null_ls.setup({
--     sources = {
--         null_ls.builtins.diagnostics.flake8
--     }
-- })
