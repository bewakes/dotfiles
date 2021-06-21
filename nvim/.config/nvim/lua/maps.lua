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
nmap('<C-w>', '<C-w>w')

-- source
nmap('<leader>s', ':Restart<CR>') -- requires nvim-reload and plenary
nmap('<leader>i', ':e ~/.config/nvim/lua/<CR>')

-- fzf
nmap('<C-p>', ':Files<CR>')

-- buffers
nmap('<C-b>', ':Buffers<CR>')
nmap('<Tab>', 'gt')
nmap('<S-Tab>', 'gT')

-- Copying to clipboard
vim.api.nvim_set_keymap('v', '<C-y>', '"+y', { noremap=true})

-- Run files
vim.cmd('nmap <leader>r :lua u.Run()<CR>')

-- lsp specific
options = { silent=true, }

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
