local o = vim.o
local wo = vim.wo
local bo = vim.bo

-- global vars
vim.g.loaded_python_provider = 0
vim.g.python3_host_prog = '~/.pyenv/shims/python'
-- vim.g.python3_host_prog = '/opt/homebrew/bin/python3'


-- global options
o.shiftwidth = 4
o.expandtab = true
o.tabstop = 4

o.splitright = true
o.smartindent = true
o.swapfile = false
o.smartcase = true
o.laststatus = 2
o.hlsearch = true
o.incsearch = true
o.ignorecase = false
o.scrolloff = 12
o.cursorline = true
o.colorcolumn = '80'
o.foldlevelstart = 10
o.list = true  -- show invisible characters
o.termguicolors = true
o.title = true
o.completeopt = 'menuone,longest,noinsert,noselect'
o.pumblend = 20

-- window-local options
wo.number = true
wo.wrap = false
wo.foldmethod = 'indent'
wo.foldenable = true
wo.winblend = 20

-- buffer-local options
bo.expandtab = true
bo.shiftwidth = 4
bo.tabstop = 4
bo.swapfile = false

vim.cmd[[
function! Build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction
]]

vim.cmd[[
let g:fzf_action = {
  \ 'ctrl-q': function('Build_quickfix_list'),
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }
]]
