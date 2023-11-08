require("mason").setup()
require("mason-lspconfig").setup({
    ensure_installed = { "rust_analyzer", "pyright", "tsserver", "lua_ls" }
})

local lsp = require('lspconfig')

local on_attach = function(client, bufnr)
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

local servers = { 'pyright', 'hls', 'gopls', 'tsserver', 'rust_analyzer', 'lua_ls' }
for _, server in pairs(servers) do
    lsp[server].setup{
        on_attach=on_attach,
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

-- 
-- local rt = require("rust-tools")
-- rt.setup({
--   server = {
--     on_attach = function(_, bufnr)
--       -- Hover actions
--       vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
--       -- Code action groups
--       vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
--     end,
--   },
-- })
