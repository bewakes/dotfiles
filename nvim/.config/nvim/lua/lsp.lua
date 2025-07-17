require("mason").setup()
require("mason-lspconfig").setup({
    ensure_installed = { 
        "pyright", 
        "ts_ls", 
        "lua_ls", 
        "clangd", 
        "hls", 
        "gopls",
        "jsonls",
        "yamlls",
        "bashls",
        "marksman",
        "taplo"
    }
})

local lsp = require('lspconfig')

local ON_ATTACH = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    vim.keymap.set('n', 'K', vim.lsp.buf.hover, {})
    vim.keymap.set('n', 'ca', vim.lsp.buf.code_action, {})
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, {})
    vim.keymap.set('n', 'gD', vim.lsp.buf.implementation, {})
    vim.keymap.set('n', 'gr', require('telescope.builtin').lsp_references, {})
    vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, {})
    vim.keymap.set('n', '<c-j>', vim.diagnostic.goto_next, {})
    vim.keymap.set('n', '<c-k>', vim.diagnostic.goto_prev, {})


    vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
      vim.lsp.handlers.hover,
      { border = 'rounded' }
    )
end

local capabilities = require("cmp_nvim_lsp").default_capabilities()

local settings = {
    rust_analyzer = {
        ["rust-analyzer"] = {
            checkOnSave = {
                enable = true,
                command = "clippy"
            },
            diagnostics = {
                enable = true
            }
        },
        server = {
            extraEnv = {
                SKIP_GUEST_BUILD = "1",
            }
        },
        cargo = {
            features = { "test_utils" }
        }
    }
}

local servers = { 
    'pyright', 
    'hls', 
    'gopls', 
    'ts_ls', 
    'rust_analyzer', 
    'lua_ls', 
    'clangd',
    'jsonls',
    'yamlls',
    'bashls',
    'marksman',
    'taplo'
}
for _, server in pairs(servers) do
    lsp[server].setup{
        on_attach=ON_ATTACH,
        capabilities=capabilities,
        flags = {
          debounce_text_changes = 150,
        },
        settings=settings[server]
    }
end

vim.diagnostic.config({
    float = {
        border = 'rounded',
    },
})
