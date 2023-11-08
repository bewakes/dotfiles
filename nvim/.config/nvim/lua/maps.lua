u = require('utils')

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
imap('<leader>c', '<C-x><C-o>')
nmap('<leader>q', ':q<CR>')
nmap('<leader>n', ':NERDTreeToggle<CR>')
nmap('<leader>bd', ':set background=dark<CR>')
nmap('<leader>bl', ':set background=light<CR>')
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

-- Copying to clipboard
vim.api.nvim_set_keymap('v', '<C-y>', '"+y', { noremap=true})

-- Run files
vim.cmd('nmap <leader>r :lua u.Run()<CR>')

-- lsp specific
options = { silent=false, }

-- nmap('<leader>e', '<cmd>lua vim.diagnostic.open_float({popup_opts={border = "single"}})<CR>', options)
-- nmap('gd','<cmd>lua vim.lsp.buf.declaration()<CR>')
-- nmap('<C-]>', '<cmd>lua vim.lsp.buf.definition()<CR>')
-- nmap('K', '<cmd>lua vim.lsp.buf.hover()<CR>')
-- nmap('gD', '<cmd>lua vim.lsp.buf.implementation()<CR>')
-- nmap('1gD', '<cmd>lua vim.lsp.buf.type_definition()<CR>')
-- nmap('gr', '<cmd>lua vim.lsp.buf.references()<CR>')
-- nmap('g0', '<cmd>lua vim.lsp.buf.document_symbol()<CR>')
-- nmap('<c-j>', '<cmd>lua vim.diagnostic.goto_next()<CR>')
-- nmap('<c-k>', '<cmd>lua vim.diagnostic.goto_prev()<CR>')


-- dap
nmap('<leader>db', "<cmd>lua require('dap').toggle_breakpoint()<CR>")
nmap('<leader>dc', "<cmd>lua require('dap').continue()<CR>")
nmap('<leader>dso', ":DapStepOver<CR>")
nmap('<leader>dsi', "<cmd>lua require('dap').step_into()<CR>")

-- vim.api.nvim_command([[
-- autocmd FileType typescriptreact nmap <C-]> <Plug>(coc-definition)
-- ]])
-- vim.api.nvim_command([[
-- autocmd FileType typescriptreact nmap <silent> K :call CocActionAsync('doHover')<CR>
-- ]])
