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
set lazyredraw                 " Redraw only when we need to
set list                       " Show invisible characters
set noswapfile                 " Disable swap file
set nowrap                     " Disable wrapping
set number                     " Shows line number
set shiftround                 " Round indent to multiple of 'shiftwidth'
set shiftwidth=4               " Number of space insert/remove shifting line
set smartindent                " Smart indentation
set autoindent
set softtabstop=4              " Number of spaces in tab when editing
set splitright                 " New windows goes right
set tabstop=4                  " Number of visual spaces per TAB
set termguicolors              " Enable 24 bit color support
set title                      " Change terminal title
set backspace=indent,eol,start
set completeopt-=preview        " do not show preview window on omnicompletion
set foldtext=MyFoldText()

set winblend=8
set pumblend=15

filetype plugin indent on      " Auto detect file type for indentation

"==============================================================================
" Plugin list
"==============================================================================
call plug#begin()

Plug 'bewakes/vim-rest-client'

Plug 'junegunn/fzf.vim'

Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'w0rp/ale'

Plug 'tpope/vim-fugitive'

Plug 'Chiel92/vim-autoformat'
Plug 'zchee/deoplete-jedi'

Plug 'justinmk/vim-dirvish'

Plug 'mhinz/vim-startify'

Plug 'scrooloose/nerdtree'
Plug 'scrooloose/nerdcommenter'

" Color schemes
Plug 'flazz/vim-colorschemes'
Plug 'ayu-theme/ayu-vim'
Plug 'sheerun/vim-polyglot'

" Git/mercurial/others diff icons on the side of the file lines
Plug 'mhinz/vim-signify'

" haskell vim
Plug 'eagletmt/ghcmod-vim'
" Plug 'neovimhaskell/haskell-vim'
Plug 'alx741/vim-hindent' " Optional

call plug#end()

"==========================================================
" Visuals, Colorschemes, ...
"==========================================================
let base16colorspace=256
set t_Co=256

set background=dark "background theme
colorscheme PaperColor

"==============================================================================
"Linter Config
"==============================================================================
let g:ale_completion_enabled = 1
let g:ale_linters = {
\   'javascript': ['eslint'],
\   'python': ['flake8'],
\   'scss': ['scsslint'],
\   'haskell': ['hlint', 'hdevtools', 'ghc-mod', 'stack-ghc'],
\   'c': ['clangtidy', 'cppcheck'],
\ }

let g:ale_fixers = {
  \ 'c': ['clang-format'],
\}

let g:ale_javascript_eslint_executable = 'eslint'

" Change error symbols
let g:ale_sign_error = '=>'
let g:ale_sign_warning = '->'

" Check on file open
let g:ale_lint_on_enter = 1

" No check on file save
let g:ale_lint_on_save = 1

" Check on text change
let g:ale_lint_on_text_changed = 1
let g:ale_lint_delay = 300 " ms

"==============================================================================
" CTRL p
"==============================================================================
let g:ctrlp_custom_ignore = 'node_modules\|.git\'


"==============================================================================
" Custom Functions
"==============================================================================
function! ToggleBackground()
    if &background=="dark"
        set background=light
    else
        set background=dark
    endif
endfunction

function! Run()
    let fullname = @%
    let path = expand('%:p:h')
    let splitted = split(fullname, "/")
    let filename = splitted[len(splitted)-1]
    let name_ext = split(filename, "\\.")
    let ext = name_ext[1]
    let name = name_ext[0]

    if ext == "py"
        exec "!python" fullname
    elseif ext == "sh"
        exec "!sh" fullname
    elseif ext == "hs"
        exec "!ghc -dynamic " fullname "-o" path."/".name
        exec "!".path."/".name
        silent exec "!rm ".path."/*.o ".path."/*.hi"
    elseif ext == "c"
        exec "!gcc " fullname "-o" path."/".name
        echo "EXECUTING..."
        exec "!".path."/".name
        silent exec "!rm ".path."/".name
    elseif ext == "js"
        exec "!node " fullname
    elseif ext == "tex"
        let command = "texi2pdf ".fullname." && rm ".path."/".name.".aux && "."rm ".path."/".name.".out && "." echo TEX converted to PDF "
        exec "!".command
    elseif ext == "rkt"
        exec "!racket ".fullname
    elseif ext == "r"
        exec "!Rscript ".fullname
    elseif ext == "vrc"
        exec "RunVrc"
    endif
endfunction

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
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
"
" CUSTOM FOLD
function! MyFoldText() " {{{
    let line = getline(v:foldstart)

    let nucolwidth = &foldcolumn + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart

    " expand tabs into spaces
    let onetab = strpart('          ', 0, &tabstop)
    let line = substitute(line, '\t', onetab, 'g')

    let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
    let fillcharcount = windowwidth - len(line) - len(foldedlinecount) - 8
    return ' > ' . line . ' ... ' . foldedlinecount . ' lines' . repeat(' ',fillcharcount)
endfunction " }}}

" CTRL-A CTRL-Q to select all and build quickfix list
function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'), 'r', { 'title': 'My search' })
  copen
  cc
endfunction
"==============================================================================
" Mappings
"==============================================================================

"==================Custom Function Mappings==================================
nnoremap <F5> :call ToggleBackground()<CR>

nnoremap <Leader>ht :GhcModType<cr>
nnoremap <Leader>htc :GhcModTypeClear<cr>

" Move to next/previous errors
nmap <silent> <leader>k <Plug>(ale_previous_wrap)
nmap <silent> <leader>j <Plug>(ale_next_wrap)

imap <leader>e <Esc>
imap <leader>E <Esc>
map <Tab> gt
nmap <Tab> gt
map <S-Tab> gT
nmap <S-Tab> gT
" Easier moving in tabs and windows
map <C-J> <C-W>j<C-W>_
map <C-K> <C-W>k<C-W>_
map <C-L> <C-W>l<C-W>_
map <C-H> <C-W>h<C-W>_
map <C-K> <C-W>k<C-W>_

nmap ;w <C-w>w
nmap ;f za
"inoremap # X#

nmap ;s :source ~/.config/nvim/init.vim<CR>
nmap ;q :q<CR>
nmap ;n :NERDTreeToggle<CR>
nmap ;l :%foldc<CR>

" Moving lines and blocks
nnoremap <A-j> :m +1<CR>
nnoremap <A-k> :m -2<CR>
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv

" Copying to clipboard
vnoremap <C-y> "+y

" buffers
nmap <C-b> :Buffers<CR>

" Spawn terminal
nmap ;t :vsplit term://bash<CR>i

" for resize
nmap <leader>vi :vertical resize +7<CR>
nmap <leader>vd :vertical resize -7<CR>
nmap <leader>hi :resize +3<CR>
nmap <leader>hd :resize -3<CR>

"fzf
nmap <C-p> :Files<CR>

" autocomplete
" Move up and down in autocomplete with <c-j> and <c-k>
inoremap <expr> <c-j> ("\<C-n>")
inoremap <expr> <c-k> ("\<C-p>")

"coc
nnoremap <silent> K :call <SID>show_documentation()<CR>
" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

nmap <leader>r :call Run()<cr>
nnoremap <silent> <leader>v :e $MYVIMRC<CR>

"==============================================================================
" Auto cmds
"==============================================================================
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

autocmd VimEnter * wincmd w
autocmd FileType haskell nnoremap <buffer> <leader>? :call ale#cursor#ShowCursorDetail()<cr>
"
" Folding 
augroup my_colors
    autocmd!
    autocmd VimEnter * highlight Folded guibg=None ctermbg=NONE cterm=italic
    autocmd ColorScheme * highlight Folded guibg=None ctermbg=NONE cterm=italic
augroup END
"
"==============================================================================
"Global variables
"==============================================================================
let g:python3_host_prog  = '/usr/bin/python3'
let g:python_host_prog  = '/usr/bin/python'

" FZF
let g:fzf_action = {
  \ 'ctrl-q': function('s:build_quickfix_list'),
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }
let $FZF_DEFAULT_OPTS = '--bind ctrl-a:select-all'

let g:airline_powerline_fonts = 1
let g:airline_theme='jellybeans'
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#tabline#formatter = 'default'
let g:Powerline_symbols = 'fancy'

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"

let g:asyncomplete_auto_popup = 1
let g:asyncomplete_remove_duplicates = 1

command! -bang -nargs=* Rg call Rg('<args>')

set exrc
set secure
