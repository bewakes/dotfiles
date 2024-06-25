local o = vim.o
local wo = vim.wo
local bo = vim.bo

-- set leader
vim.g.mapleader = ';'

-- global vars
vim.g.loaded_python_provider = 0
vim.g.python3_host_prog = '/Users/bibek/common_venv/bin/python'

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
o.pumblend = 10

-- window-local options
wo.number = true
wo.wrap = false
wo.foldmethod = 'indent'
wo.foldenable = true
wo.winblend = 10

-- buffer-local options
bo.expandtab = true
bo.shiftwidth = 4
bo.tabstop = 4
bo.swapfile = false
