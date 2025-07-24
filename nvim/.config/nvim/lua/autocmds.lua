vim.api.nvim_create_augroup('AutoFormatting', {})

-- Auto-format supported file types on save
local format_filetypes = {
    '*.rs',
    '*.hs',
    '*.py',
    '*.lua',
    '*.js',
    '*.ts',
    '*.jsx',
    '*.tsx',
    '*.go',
    '*.c',
    '*.cpp',
    '*.h',
    '*.hpp',
    '*.json',
    '*.yaml',
    '*.yml',
    '*.sh',
    '*.md',
    '*.toml'
}

vim.api.nvim_create_autocmd('BufWritePre', {
  pattern = format_filetypes,
  group = 'AutoFormatting',
  callback = function()
    vim.lsp.buf.format({ async = false })
  end,
})
