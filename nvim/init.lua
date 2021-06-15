vim.o.colorcolumn = '80'
vim.o.cursorline = true
vim.o.expandtab = true
vim.o.foldenable = true
vim.o.foldlevelstart = 10
vim.o.foldmethod = 'indent'
vim.o.hidden = true
vim.o.lazyredraw = true
vim.o.list = true -- Show invisible characters
vim.api.nvim_command('set number')
vim.api.nvim_command('set nowrap')
vim.o.smartindent = true
vim.o.splitright = true
vim.o.termguicolors = true
vim.o.title = true
vim.o.backspace = 'indent,eol,start'
vim.o.winblend = 5
vim.o.pumblend = 10
vim.o.swapfile = false
vim.o.guifont = 'DejaVu Sans Mono:12'

vim.o.completeopt = 'menuone,longest,noinsert,noselect'

vim.g.airline_powerline_fonts = 1
vim.g.python3_host_prog = '/home/bibek/.pyenv/versions/3.9.0b5/bin/python'

local vapi = vim.api
local cmd = vapi.nvim_command

cmd('set expandtab')
cmd('set shiftwidth=4')
cmd('set tabstop=4')
cmd('set softtabstop=4')

-- LEADER
cmd('let mapleader=";"')

local plug = function(plugin)
	cmd("Plug " .. "'" .. plugin .. "'")
end


cmd("call plug#begin()")

cmd("Plug 'junegunn/fzf', { 'do': './install --bin' }")

plug("bewakes/vim-rest-client")
-- plug("./plugged/vim-nepali-unicode/")
plug("bewakes/secrets-vim")
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
-- plug("parsonsmatt/intero-neovim")    -- For Ghci REPL
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

-- vim.g.ale_linters = {
--     javascript= {'eslint'},
--     python= {'flake8', 'mypy'},
--     haskell= {'hlint', 'ghc-mod'},
-- }
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
vapi.nvim_set_keymap('v', '<C-y>', '"+y', { noremap=true})

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
nmap('gd','<cmd>lua vim.lsp.buf.declaration()<CR>', { silent=true, noremap=true })
nmap('<c-]>', '<cmd>lua vim.lsp.buf.definition()<CR>', { silent=true, noremap=true })
nmap('K', '<cmd>lua vim.lsp.buf.hover()<CR>', { silent=true, noremap=true })
nmap('gD', '<cmd>lua vim.lsp.buf.implementation()<CR>', { silent=true, noremap=true })
nmap('1gD', '<cmd>lua vim.lsp.buf.type_definition()<CR>', { silent=true, noremap=true })
nmap('gr', '<cmd>lua vim.lsp.buf.references()<CR>', { silent=true, noremap=true })
nmap('g0', '<cmd>lua vim.lsp.buf.document_symbol()<CR>', { silent=true, noremap=true })
-- Auto commands


-- LSP
local lsp = require('nvim_lsp')

lsp.hls.setup{}
lsp.pyls.setup{}

-- Close preview after auto complete done
cmd('autocmd CompleteDone * pclose!')


-- HASKELL
--
require('nvim_lsp').hls.setup{on_attach=require('completion').on_attach}

-- TODO: custom functions
-- Run
Run = function ()
    local len = function(tbl)
      local getN = 0
      for n in pairs(tbl) do 
        getN = getN + 1 
      end
      return getN
    end
    local Split = function(s, delimiter)
         result = {};
         for match in (s..delimiter):gmatch("(.-)"..delimiter) do
             table.insert(result, match);
         end
         return result;
    end

    local SplitByDot = function(s)
	result = {};
	for match in s:gmatch("([^.]+)") do
	    table.insert(result, match)
	end
	return result;
    end

    local eval = vim.api.nvim_eval
    local fullname = eval('@%')
    local path = eval('expand("%:p:h")')
    local splitted = Split(fullname, "/") --eval('split("'..fullname..'", "/")')
    local filename = splitted[len(splitted)]
    local name_ext = SplitByDot(filename)
    local ext = name_ext[2]
    local name = name_ext[1]
    local fullpath = "'"..path.."/"..name.."'"

    if ext == "py" then
	cmd('exec "!time python '..fullname..'"')
    elseif ext == "sh" then
	cmd('exec "!time sh '..fullname..'"')
    elseif ext == "hs" then
	cmd('exec "!time stack runhaskell '..fullname..'"')
	-- TODO: remove *.o and *.hi
    elseif ext == "c" then
	cmd('exec "!gcc '..fullname..' -o '..path..'/'..name..'"')
	cmd('exec "!time '..path..'/'..name..'"')
    elseif ext == "js" or ext == "ts" or ext == "tsx" then
    cmd('exec "!node '..path..'/'..name..'"')
    elseif ext == "tex" then
        cmd('exec "!texi2pdf '..fullname..'"')
    elseif ext == "rkt" then
    elseif ext == "r" then
    elseif ext == "vrc" then
        cmd('exec "RunVrc"')
    end
end

cmd('nmap <leader>r :lua Run()<CR>')
