local lsp = require('lspconfig')
local util = require("lspconfig/util")

local on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    local bufopts = { noremap=true, silent=true, buffer=bufnr }
    -- vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
    -- vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
    -- vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
    -- vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
    -- vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
    -- vim.keymap.set('n', '<space>K', vim.lsp.buf.signature_help, bufopts)
    -- vim.keymap.set('n', 'gt', vim.lsp.buf.type_definition, bufopts)
    -- vim.keymap.set('n', '<F2>', vim.lsp.buf.rename, bufopts)
    -- vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
    -- vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
    -- vim.keymap.set('n', '<space>f', vim.lsp.buf.formatting, bufopts)
    -- vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
    -- vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
    -- vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
end

local servers = { 'pyright', 'hls', 'gopls', 'tsserver' }
for _, server in pairs(servers) do
    lsp[server].setup{
        on_attach=on_attach,
        flags = {
          debounce_text_changes = 150,
        }
    }
end

-- lsp.gopls.setup {
--     cmd = {"gopls", "serve"},
--     filetypes = {"go", "gomod"},
--     root_dir = util.root_pattern("go.work", "go.mod", ".git"),
--     settings = {
--       gopls = {
--         analyses = {
--           unusedparams = true,
--         },
--         staticcheck = true,
--       },
--     },
-- }

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
        virtual_text = true,
        underline = true,
        signs = true,
    }
)


local null_ls = require("null-ls")
null_ls.setup({
    sources = {
        null_ls.builtins.diagnostics.flake8
    }
})
