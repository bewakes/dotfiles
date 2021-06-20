local o = vim.o
local wo = vim.wo
local bo = vim.bo

-- global vars
vim.g.airlinei_powerline_fonts = 1
vim.g.python3_host_prog = 'python'


-- global options
o.splitright = true
o.smartindent = true
o.swapfile = false
o.smartcase = true
o.laststatus = 2
o.hlsearch = true
o.incsearch = true
o.ignorecase = true
o.scrolloff = 12
o.cursorline = true
o.colorcolumn = '80'
o.foldlevelstart = 10
o.list = true  -- show invisible characters
o.termguicolors = true
o.title = true
o.completeopt = 'menuone,longest,noinsert'
o.pumblend = 10


-- window-local options
wo.number = true
wo.wrap = false
wo.foldmethod = 'indent'
wo.foldenable = true
wo.winblend = 5

-- buffer-local options
bo.expandtab = true
bo.shiftwidth = 4
bo.tabstop = 4
bo.swapfile = false


-- colorscheme
vim.api.nvim_command('colorscheme solarized8')
