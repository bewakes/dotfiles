"==============================================================================
" General vim settings
"==============================================================================

let mapleader=";"
set colorcolumn=80             " Ruler for maximum characters
set cursorline                 " Highlight current line
set expandtab                  " Tabs are spaces
set foldenable                 " Enable folding
set foldlevelstart=10          " Open most folds by default
set foldmethod=indent          " Fold based on indent level
set hidden                     " Enables hidden buffer
"set ignorecase                 " Case insensitive search
set lazyredraw                 " Redraw only when we need to
set list                       " Show invisible characters
set noswapfile                 " Disable swap file
set nowrap                     " Disable wrapping
set number                     " Shows line number
"set relativenumber             " Enable relative numbering
set shiftround                 " Round indent to multiple of 'shiftwidth'
set shiftwidth=4               " Number of space insert/remove shifting line
"set shortmess=I                " Hide intro message
"set smartcase                  " Performs case sensitive search if contains uppercase letters
"set smartindent                " Smart indentation
set softtabstop=4              " Number of spaces in tab when editing
set splitright                 " New windows goes right
set tabstop=4                  " Number of visual spaces per TAB
set termguicolors              " Enable 24 bit color support
set title                      " Change terminal title
"set undofile                   " Persistent undo
"set undolevels=1000            " How many undos
"set undoreload=10000           " Number of lines to save for undo
set backspace=indent,eol,start

"let g:python3_host_prog  = '/usr/bin/python3'

" Move to next/previous errors
nmap <silent> <leader>k <Plug>(ale_previous_wrap)
nmap <silent> <leader>j <Plug>(ale_next_wrap)




"==============================================================================
" Plugin list
"==============================================================================
call plug#begin()

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'leafgarland/typescript-vim'
Plug 'isRuslan/vim-es6'

Plug 'majutsushi/tagbar'
Plug 'w0rp/ale'

Plug 'Lokaltog/vim-powerline'

Plug 'tpope/vim-fugitive'
"Plug 'airblade/vim-gitgutter'
"Plug 'int3/vim-extradite'
"Plug 'neomake/neomake'
Plug 'Shougo/deoplete.nvim'
"Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'mileszs/ack.vim'

Plug 'othree/html5.vim'
Plug 'othree/javascript-libraries-syntax.vim'
Plug 'pangloss/vim-javascript'
Plug 'Chiel92/vim-autoformat'
Plug 'zchee/deoplete-jedi'

Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-eunuch'
Plug 'junegunn/vim-peekaboo'
"Plug 'junegunn/vim-easy-align', { 'on': ['<Plug>(EasyAlign)', 'EasyAlign'] }
Plug 'justinmk/vim-dirvish'
Plug 'justinmk/vim-sneak'
Plug 'wellle/targets.vim'
Plug 'kshenoy/vim-signature'
Plug 'tmhedberg/matchit'
"Plug 'mbbill/undotree', { 'on': 'UndotreeToggle' }

Plug 'mhinz/vim-startify'
Plug 'Yggdroot/indentLine'

Plug 'justinmk/vim-gtfo'
Plug 'tpope/vim-dispatch'
Plug 'christoomey/vim-tmux-navigator'

Plug 'editorconfig/editorconfig-vim'
Plug 'kien/ctrlp.vim'

Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'

Plug 'flazz/vim-colorschemes'

Plug 'terryma/vim-multiple-cursors'

Plug 'editorconfig/editorconfig-vim'

Plug 'nvie/vim-flake8'

Plug 'vim-scripts/wombat256.vim'
Plug 'rafi/awesome-vim-colorschemes'

Plug 'diepm/vim-rest-console'

"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
call plug#end()

let g:ctrlp_custom_ignore = 'node_modules\|.git\'

" js
let g:ale_linters = {
\   'javascript': ['eslint'],
\}
"let g:jsx_ext_required = 0

let g:ale_javascript_eslint_executable = 'eslint'

" Change error symbols
let g:ale_sign_error = '=>'
let g:ale_sign_warning = '->'

" Check on file open
let g:ale_lint_on_enter = 1

" No check on file save
let g:ale_lint_on_save = 0

" Check on text change
let g:ale_lint_on_text_changed = 1
let g:ale_lint_delay = 300 " ms


"==============================================================================
" Colorscheme
"==============================================================================

set background=dark

let g:gruvbox_invert_selection=0
let g:gruvbox_contrast_dark='soft'

colorscheme afterglow

function! ToggleBackground()
    if &background=="dark"
        set background=light
    else
        set background=dark
    endif
endfunction

nnoremap <F5> :call ToggleBackground()<CR>

"==============================================================================
" Mappings
"==============================================================================

imap fk <Esc>
imap FK <Esc>
imap fj <C-n>
imap FJ <C-n>
map <Tab> gt
nmap <Tab> gt
map <S-Tab> gT
nmap <S-Tab> gT
"map <S-Tab> :tabl<cr>
"nmap <S-Tab> :tabl<cr>
nmap ;w <C-w>w
nmap ;f za
inoremap # X#

nmap ;s :source ~/.config/nvim/init.vim<CR>
nmap ;q :q<CR>
nmap ;n :NERDTreeToggle<CR>
nmap ;l :%foldc<CR>

" Spawn terminal
nmap ;t :vsplit term://bash<CR>i

" C-tags shortcuts
nmap ;g <C-]>
nmap ;b <C-t>

" for resize
nmap <leader>vi :vertical resize +7<CR>
nmap <leader>vd :vertical resize -7<CR>
nmap <leader>hi :resize +3<CR>
nmap <leader>hd :resize -3<CR>

" for terminal mode
tnoremap <C-n> <C-\><C-n>


autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

autocmd VimEnter * wincmd w
"autocmd BufWinEnter * NERDTreeMirror
 
"Pathogen
"execute pathogen#infect()

":let g:nerdtree_tabs_open_on_console_startup = 1

let g:airline_powerline_fonts = 1
let g:airline_theme='jellybeans'
let g:airline#extensions#tabline#enabled = 1
let g:Powerline_symbols = 'fancy'
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"
"
function! Run()
    let fullname = @%
    let splitted = split(fullname, "/")
    let filename = splitted[len(splitted)-1]
    let name_ext = split(filename, "\\.")
    let ext = name_ext[1]
    let name = name_ext[0]

    if ext == "py"
        exec "!python" fullname
    elseif ext == "hs"
        silent exec "!ghc" filename "-o" name
        exec "!./".name
        silent exec "!rm *.o *.hi"
    endif
    "!python %
endfunction

nmap ;r :call Run()<cr>
