vim.o.colorcolumn = '80'
vim.o.cursorline = true
vim.o.expandtab = true
vim.o.foldenable = true
vim.o.foldlevelstart = 10
vim.o.foldmethod = 'indent'
vim.o.hidden = true
vim.o.lazyredraw = true
vim.o.list = true -- Show invisible characters
-- vim.o.noswapfile = true
-- vim.o.nowrap = true
vim.api.nvim_command('set number')
vim.o.smartindent = true
vim.o.shiftwidth = 4
vim.o.splitright = true
vim.o.tabstop = 4
vim.o.termguicolors = true
vim.o.title = true
vim.o.backspace = 'indent,eol,start'
vim.o.winblend = 5
vim.o.pumblend = 10

vim.g.airline_powerline_fonts = 1
vim.g.python3_host_prog = '/home/bibek/.pyenv/versions/3.9.0b5/bin/python'

local vapi = vim.api
local cmd = vapi.nvim_command

-- LEADER
cmd('let mapleader=";"')

local plug = function(plugin)
	cmd("Plug " .. "'" .. plugin .. "'")
end


cmd("call plug#begin()")

cmd("Plug 'junegunn/fzf', { 'do': './install --bin' }")

-- plug("bewakes/vim-rest-client")
-- plug("bewakes/secrets-vim")
plug("w0rp/ale")
plug("junegunn/fzf.vim")
plug("neomake/neomake")
plug("scrooloose/nerdtree")              -- Directory Tree
plug("airblade/vim-gitgutter")           -- Git diffs
plug("tpope/vim-fugitive")           -- Git diffs
plug("godlygeek/tabular")                -- Text filtering and alignment
plug("dhruvasagar/vim-table-mode")
plug("vim-airline/vim-airline")          -- Status Bar
plug("justinmk/vim-dirvish")             -- Directory viewer

-- Color Scheme
plug("dkasak/gruvbox")
plug("lifepillar/vim-solarized8")

-- LSP client
plug("neovim/nvim-lsp")
plug("neovim/nvim-lspconfig")
cmd("Plug 'neoclide/coc.nvim', {'branch': 'release'}")

-- completion
plug("nvim-lua/completion-nvim")

-- haskell
plug("raichoo/haskell-vim")
plug("parsonsmatt/intero-neovim")    -- For Ghci REPL
plug("alx741/vim-stylishask")        -- Stylizing code, requires stack install stylish-haskell

-- js/ts
plug("maxmellon/vim-jsx-pretty")
plug("pangloss/vim-javascript")
plug("neoclide/vim-jsx-improve")
plug("leafgarland/typescript-vim")
plug("ianks/vim-tsx")

-- python
-- plug("zchee/deoplete-jedi")

cmd("call plug#end()")


-- color scheme
cmd("colorscheme solarized8")
vim.o.background = 'dark'

-- KEY BINDINGS
local nmap = function (k, a)
	vapi.nvim_set_keymap('n', k, a, {})
end

local imap = function (k, a)
	vapi.nvim_set_keymap('i', k, a, {})
end

local map = function (k, a)
	vapi.nvim_set_keymap('', k, a, {})
end

imap('<leader>e', '<Esc>')
nmap('<leader>q', ':q<CR>')
nmap('<leader>n', ':NERDTreeToggle<CR>')
nmap('<leader>r', ':call Run()<CR>')
-- nmap('<leader>s', ':source $MYVIMRC<CR>')
-- nmap('<leader>v', ':vs $MYVIMRC<CR>')
nmap('<C-w>', '<C-w>w')

-- Copying to clipboard
vapi.nvim_set_keymap('v', '<C-y>', '+y"', { noremap=true})

-- fzf
nmap('<C-p>', ':Files<CR>')
-- buffers
nmap('<C-b>', ':Buffers<CR>')

-- vapi.nvim_set_keymap('t', '<Esc>', 'C-\><C-n>', { noremap=true})
map('<Tab>', 'gt')
nmap('<Tab>', 'gt')
map('<S-Tab>', 'gT')
nmap('<S-Tab>', 'gT')

-- Move to next/previous errors
-- nmap <silent> <leader>k <Plug>(ale_previous_wrap)
-- nmap <silent> <leader>j <Plug>(ale_next_wrap)

-- command! -bang -nargs=* Rg call Rg('<args>')
--
-- TODO: lsp key bindings

-- Auto commands


-- LSP
local lsp = require('nvim_lsp')

lsp.hls.setup{}
lsp.pyls.setup{}


-- HASKELL
vim.g.intero_start_immediately = 1

-- TODO: custom functions
-- Run
