require("mason").setup()
require("mason-lspconfig").setup({
    ensure_installed = { "pyright", "tsserver", "lua_ls", "clangd" }
})

local lsp = require('lspconfig')

local ON_ATTACH = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    vim.keymap.set('n', 'K', vim.lsp.buf.hover, {})
    vim.keymap.set('n', 'ca', vim.lsp.buf.code_action, {})
    vim.keymap.set('n', '<C-]>', vim.lsp.buf.definition, {})
    vim.keymap.set('n', 'gD', vim.lsp.buf.implementation, {})
    vim.keymap.set('n', 'gr', require('telescope.builtin').lsp_references, {})
    vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, {})
    vim.keymap.set('n', '<c-j>', vim.diagnostic.goto_next, {})
    vim.keymap.set('n', '<c-k>', vim.diagnostic.goto_prev, {})
end

local capabilities = require("cmp_nvim_lsp").default_capabilities()

local servers = { 'pyright', 'hls', 'gopls', 'tsserver', 'rust_analyzer', 'lua_ls', 'clangd' }
for _, server in pairs(servers) do
    lsp[server].setup{
        on_attach=ON_ATTACH,
        capabilities=capabilities,
        flags = {
          debounce_text_changes = 150,
        }
    }
end

vim.diagnostic.config({
    float = {
        border = 'rounded',
    },
})

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = "rounded"
})

-- local rt = require('rust-tools')
-- rt.setup({
--     server = {
--         on_attach = function(client, bufnr)
--             ON_ATTACH(client, bufnr)
--             vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
--         end,
--         settings = {
--           ["rust-analyzer"] = {
--             -- enable clippy on save
--             checkOnSave = {
--               command = "+1.78.0 clippy --all --all-features --workspace --tests --benches --examples",
--               -- command = "clippy",
--             },
--           },
--         },
--     }
-- })
