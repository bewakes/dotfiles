" ==============================================================================
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
set lazyredraw                 " Redraw only when we need to
set list                       " Show invisible characters
set noswapfile                 " Disable swap file
set nowrap                     " Disable wrapping
set number                     " Shows line number
set shiftwidth=4               " Number of space insert/remove shifting line
set smartindent                " Smart indentation
set autoindent
set softtabstop=4              " Number of spaces in tab when editing
set splitright                 " New windows goes right
set tabstop=4                  " Number of visual spaces per TAB
set termguicolors              " Enable 24 bit color support
set title                      " Change terminal title
set backspace=indent,eol,start

set winblend=5
set pumblend=10

syntax on
filetype plugin indent on      " Auto detect file type for indentation

"==============================================================================
" Plugins
"==============================================================================
call plug#begin()

Plug 'bewakes/vim-rest-client'
Plug 'bewakes/secrets-vim'
Plug 'w0rp/ale'
Plug 'junegunn/fzf', { 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'neomake/neomake'
Plug 'scrooloose/nerdtree'              " Directory Tree
Plug 'airblade/vim-gitgutter'           " Git diffs
Plug 'tpope/vim-fugitive'           " Git diffs
Plug 'godlygeek/tabular'                " Text filtering and alignment
Plug 'dhruvasagar/vim-table-mode'
Plug 'vim-airline/vim-airline'          " Status Bar
Plug 'justinmk/vim-dirvish'             " Directory viewer

" COLORSCHEMES
Plug 'dkasak/gruvbox'
Plug 'lifepillar/vim-solarized8'

" LSP Client
Plug 'neovim/nvim-lsp'
Plug 'neovim/nvim-lspconfig'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Completion
Plug 'nvim-lua/completion-nvim'
" HASKELL
" Plug 'neovimhaskell/haskell-vim'    " For syntax highlighting and auto indentation
Plug 'raichoo/haskell-vim'
Plug 'parsonsmatt/intero-neovim'    " For Ghci REPL
Plug 'alx741/vim-stylishask'        " Stylizing code, requires stack install stylish-haskell

" Javascript/Typescript
Plug 'maxmellon/vim-jsx-pretty'
Plug 'pangloss/vim-javascript'
Plug 'neoclide/vim-jsx-improve'
Plug 'leafgarland/typescript-vim'
Plug 'ianks/vim-tsx'

" PYTHON
" Plug 'zchee/deoplete-jedi'          " Requires pip install jedi

call plug#end()

"==============================================================================
" Color scheme
"==============================================================================
colorscheme solarized8
set background=dark "background theme

"==============================================================================
" General key bindings
"==============================================================================
imap <leader>e <Esc>
nmap <leader>q :q<CR>
nmap <leader>n :NERDTreeToggle<CR>
nmap <leader>r :call Run()<CR>
nmap <leader>s :source $MYVIMRC<CR>
nmap <leader>v :vs $MYVIMRC<CR>
nmap <C-w> <C-w>w
" Easier moving in tabs and windows
map <C-J> <C-W>j<C-W>_
map <C-K> <C-W>k<C-W>_
map <C-L> <C-W>l<C-W>_
map <C-H> <C-W>h<C-W>_
map <C-K> <C-W>k<C-W>_
" Copying to clipboard
vnoremap <C-y> "+y
"fzf
nmap <C-p> :Files<CR>
" buffers
nmap <C-b> :Buffers<CR>

tnoremap <Esc> <C-\><C-n>
map <Tab> gt
nmap <Tab> gt
map <S-Tab> gT
nmap <S-Tab> gT

" Move to next/previous errors
nmap <silent> <leader>k <Plug>(ale_previous_wrap)
nmap <silent> <leader>j <Plug>(ale_next_wrap)

command! -bang -nargs=* Rg call Rg('<args>')

" LSP keybindings
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>


"==============================================================================
" Auto commands
"==============================================================================
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
augroup SyntaxSettings
    autocmd!
    autocmd BufNewFile,BufRead *.tsx set filetype=typescript.tsx
augroup END

" Close preview after auto complete done
autocmd CompleteDone * pclose!

" Call NeoMake every time file is saved
call neomake#configure#automake('w')
" Automatically open window listing issues instead of triggering by :lopen
let g:neomake_open_list = 0
let g:airline_powerline_fonts = 1

" let g:python3_host_prog = '/usr/bin/python3'
let g:python3_host_prog = '/home/bibek/.pyenv/versions/3.9.0b5/bin/python'


" Haskell specific
" =============================================================================
let g:stylishask_on_save = 0
let g:stylishask_config_file = '~/.stylish-haskell.yaml'
let g:hindent_on_save = 0
let g:intero_start_immediately = 1
let g:intero_backend = { 'command': 'stack ghci' }


" Auto reload intero on save
autocmd BufWrite *.hs InteroReload

" Lookup the type of expression under the cursor
au FileType haskell nmap <silent> <leader>t <Plug>InteroGenericType
" Insert type declaration
au FileType haskell nnoremap <silent> <leader>ni :InteroTypeInsert<CR>
" Show info about expression or type under the cursor
au FileType haskell nnoremap <silent> <leader>i :InteroInfo<CR>

" Jump to the definition of an identifier
au FileType haskell nnoremap <silent> <leader>ng :InteroGoToDef<CR>
" Reboot Intero, for when dependencies are added
au FileType haskell nnoremap <silent> <leader>nr :InteroKill<CR> :InteroOpen<CR>

au FileType haskell nnoremap <silent> <leader>ps :Stylishask<CR>

" haskell-vim
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
" =============================================================================


" LSP CONFIG
lua require'nvim_lsp'.hls.setup{on_attach=require'completion'.on_attach}
"lua require'nvim_lsp'.tsserver.setup{}
lua require'nvim_lsp'.pyls.setup{on_attach=require'completion'.on_attach}
set completeopt-=preview
set completeopt=menuone,noinsert,noselect

" use omni completion provided by lsp
autocmd Filetype python setlocal omnifunc=v:lua.vim.lsp.omnifunc

let g:ale_linters = {
    \ 'javascript': ['eslint'],
    \ 'python': ['flake8', 'mypy'],
    \ 'haskell': ['hlint', 'ghc-mod'],
\}

let g:ale_fixers = {
    \ 'python': ['yapf'],
    \ 'java': ['uncrustify'],
    \ 'javascript': ['eslint'],
\}

" FZF
" CTRL-A CTRL-Q to select all and build quickfix list
function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'), 'r', { 'title': 'My search' })
  copen
  cc
endfunction

let g:fzf_action = {
  \ 'ctrl-q': function('s:build_quickfix_list'),
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }
let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'


"===============================================================================
" Custom functions
"===============================================================================

function! Run()
    let fullname = @%
    let path = expand('%:p:h')
    let splitted = split(fullname, "/")
    let filename = splitted[len(splitted)-1]
    let name_ext = split(filename, "\\.")
    let ext = name_ext[1]
    let name = name_ext[0]
    let fullpath = "'".path."/".name."'"

    if ext == "py"
        exec "!time python" fullname
    elseif ext == "sh"
        exec "!time sh" fullname
    elseif ext == "hs"
        exec "!time stack runhaskell " fullname
        " exec "!".fullpath
        silent exec "!rm '".path."/*.o' '".path."/*.hi'"
    elseif ext == "c"
        exec "!gcc " fullname "-o" path."/".name
        echo "EXECUTING..."
        exec "!time ".path."/".name
        silent exec "!rm ".path."/".name
    elseif ext == "js" || ext == "ts" || ext == "tsx"
        exec "!time node " fullname
    elseif ext == "tex"
        let command = "texi2pdf ".fullname." && rm ".path."/".name.".aux && "."rm ".path."/".name.".log && "."rm ".path."/".name.".fls &&"." echo TEX converted to PDF "
        exec "!".command
    elseif ext == "rkt"
        exec "!time racket ".fullname
    elseif ext == "r"
        exec "!time Rscript ".fullname
    elseif ext == "vrc"
        exec "RunVrc"
    endif
endfunction

fun! Rg(arg) "{{{ 
    let s:query = a:arg
    if empty(a:arg)
        let s:query = expand("<cword>")
    endif
    call fzf#vim#grep(
    \   'rg --column --line-number --no-heading --color=always '.shellescape(s:query), 1,
    \   fzf#vim#with_preview('up:60%')
    \   )
endfunction "}}}

set exrc
set secure
