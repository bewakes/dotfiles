local u = require('utils')

local opts = { noremap = true, silent = true }

-- Insert mode mappings
vim.keymap.set('i', '<leader>e', '<Esc>', opts)
vim.keymap.set('i', '<leader>c', '<C-x><C-o>', opts)

-- Normal mode mappings
vim.keymap.set('n', '<leader>q', ':q<CR>', opts)
vim.keymap.set('n', '<leader>w', ':w<CR>', opts)
vim.keymap.set('n', '<leader>gb', ':Git blame<CR>', opts)
vim.keymap.set('n', '<leader>lf', vim.lsp.buf.format, opts)
vim.keymap.set('n', '<leader>lr', vim.lsp.buf.rename, opts)
vim.keymap.set('n', '<leader>n', ':NvimTreeToggle<CR>', opts)
vim.keymap.set('n', '<leader>bd', ':set background=dark<CR>', opts)
vim.keymap.set('n', '<leader>bl', ':set background=light<CR>', opts)
vim.keymap.set('n', '<leader>z', 'za', opts)
vim.keymap.set('n', '<Tab>', 'gt', opts)
vim.keymap.set('n', '<S-Tab>', 'gT', opts)
vim.keymap.set('n', 'gv', '<C-o>', opts)

-- Window commands
vim.keymap.set('n', '<leader>h', '<C-w>h', opts)
vim.keymap.set('n', '<leader>j', '<C-w>j', opts)
vim.keymap.set('n', '<leader>k', '<C-w>k', opts)
vim.keymap.set('n', '<leader>l', '<C-w>l', opts)
vim.keymap.set('n', '<leader>=', '<C-w>=', opts)

-- nb commands
vim.keymap.set('n', '<Leader>ww', ':!nb browse <CR>', opts)
vim.keymap.set('n', '<Leader>wi', ':!nb browse %:p:h:t/%:t <CR>', opts)
vim.keymap.set('n', '<Leader>wt', ':e `="~/.nb/home/Journal/" . expand(strftime("%Y-%m-%d")) . ".md"` <CR>', opts)
vim.keymap.set('n', '<Leader>wm', ':e `="~/.nb/home/Journal/" . expand(strftime("%Y-%m")) . ".md"` <CR>', opts)

-- Telescope
vim.keymap.set('n', '<leader>s', ':Telescope lsp_document_symbols<CR>', opts)

-- Config editing
vim.keymap.set('n', '<leader>i', ':e ~/.config/nvim/lua/<CR>', opts)

-- Copying to clipboard
vim.keymap.set('v', '<C-y>', '"+y', opts)

-- Run files
vim.keymap.set('n', '<leader>r', u.Run, opts)

-- DAP (Debug Adapter Protocol)
vim.keymap.set('n', '<leader>db', function() require('dap').toggle_breakpoint() end, opts)
vim.keymap.set('n', '<leader>dc', function() require('dap').continue() end, opts)
vim.keymap.set('n', '<leader>dso', ':DapStepOver<CR>', opts)
vim.keymap.set('n', '<leader>dsi', ':DapStepInto<CR>', opts)

-- Neotest
vim.keymap.set('n', '<leader>tn', ':Neotest run<CR>', opts)
vim.keymap.set('n', '<leader>to', ':Neotest output<CR>', opts)

-- Trouble (diagnostics)
vim.keymap.set('n', '<leader>tt', ':Trouble<CR>', opts)
vim.keymap.set('n', '<leader>tw', ':Trouble workspace_diagnostics<CR>', opts)
vim.keymap.set('n', '<leader>td', ':Trouble document_diagnostics<CR>', opts)
vim.keymap.set('n', '<leader>tq', ':Trouble quickfix<CR>', opts)

-- Clear search highlighting
vim.keymap.set('n', '<leader>/', ':nohlsearch<CR>', opts)
