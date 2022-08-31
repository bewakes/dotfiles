u = require('utils')

-- set leader
vim.g.mapleader = ';'

local nmap = function (k, a)
	vim.api.nvim_set_keymap('n', k, a, {})
end

local imap = function (k, a)
	vim.api.nvim_set_keymap('i', k, a, {})
end

local map = function (k, a)
	vim.api.nvim_set_keymap('', k, a, {})
end

imap('<leader>e', '<Esc>')
nmap('<leader>q', ':q<CR>')
nmap('<leader>n', ':NERDTreeToggle<CR>')
nmap('<leader>d', ':set background=dark<CR>')
nmap('<leader>b', ':set background=light<CR>')
nmap('<Tab>', 'gt')
nmap('<S-Tab>', 'gT')
-- Window commands
nmap('<leader>h', '<C-w>h')
nmap('<leader>j', '<C-w>j')
nmap('<leader>k', '<C-w>k')
nmap('<leader>l', '<C-w>l')
nmap('<leader>=', '<C-w>=')
-- nb commands
nmap('<Leader>ww', ':!nb browse <CR>')
nmap('<Leader>wi', ':!nb browse %:p:h:t/%:t <CR>')
nmap('<Leader>wt', ':e `="~/.nb/home/Journal/" . expand(strftime("%Y-%m-%d")) . ".md"` <CR>')
nmap('<Leader>wm', ':e `="~/.nb/home/Journal/" . expand(strftime("%Y-%m")) . ".md"` <CR>')
-- nmap <Leader>wh :!nb export %:p:h:t/%:t ~/_site/%:t:r.html <CR>

-- source
nmap('<leader>s', ':Restart<CR>') -- requires nvim-reload and plenary
nmap('<leader>i', ':e ~/.config/nvim/lua/<CR>')

-- fzf
nmap('<C-p>', ':Files<CR>')

-- buffers
nmap('<C-b>', ':Buffers<CR>')

-- Copying to clipboard
vim.api.nvim_set_keymap('v', '<C-y>', '"+y', { noremap=true})

-- Run files
vim.cmd('nmap <leader>r :lua u.Run()<CR>')

-- lsp specific
options = { silent=false, }

nmap('<leader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', options)
nmap('gd','<cmd>lua vim.lsp.buf.declaration()<CR>', options)
nmap('<c-]>', '<cmd>lua vim.lsp.buf.definition()<CR>', options)
nmap('K', '<cmd>lua vim.lsp.buf.hover()<CR>', options)
nmap('gD', '<cmd>lua vim.lsp.buf.implementation()<CR>', options)
nmap('1gD', '<cmd>lua vim.lsp.buf.type_definition()<CR>', options)
nmap('gr', '<cmd>lua vim.lsp.buf.references()<CR>', options)
nmap('g0', '<cmd>lua vim.lsp.buf.document_symbol()<CR>', options)
nmap('g0', '<cmd>lua vim.lsp.buf.document_symbol()<CR>', options)
nmap('<c-j>', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', options)
nmap('<c-k>', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', options)

vim.api.nvim_command([[
autocmd FileType typescriptreact nmap <C-]> <Plug>(coc-definition)
]])
vim.api.nvim_command([[
autocmd FileType typescriptreact nmap <silent> K :call CocActionAsync('doHover')<CR>
]])
