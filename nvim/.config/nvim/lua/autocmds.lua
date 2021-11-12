vim.api.nvim_exec([[
autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()
]], false)
