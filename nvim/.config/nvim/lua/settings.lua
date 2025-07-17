-- set leader
vim.g.mapleader = ';'

-- global vars
vim.g.loaded_python_provider = 0

-- Dynamic Python path detection
local function find_python3()
    local python_paths = {
        '/Users/bibek/venv/bin/python3',
        vim.fn.exepath('python3'),
        vim.fn.exepath('python'),
        '/usr/bin/python3',
        '/usr/local/bin/python3',
    }
    
    for _, path in ipairs(python_paths) do
        if vim.fn.executable(path) == 1 then
            return path
        end
    end
    return 'python3'
end

vim.g.python3_host_prog = find_python3()

-- Use vim.opt for modern, consistent settings
local opt = vim.opt

-- Indentation
opt.shiftwidth = 4
opt.expandtab = true
opt.tabstop = 4
opt.smartindent = true

-- UI/UX
opt.splitright = true
opt.splitbelow = true
opt.swapfile = false
opt.backup = false
opt.undofile = true
opt.undodir = os.getenv('HOME') .. '/.vim/undodir'
opt.laststatus = 2
opt.scrolloff = 12
opt.sidescrolloff = 8
opt.cursorline = true
opt.colorcolumn = '80'
opt.signcolumn = 'yes'
opt.wrap = false
opt.number = true
opt.relativenumber = false
opt.termguicolors = true
opt.title = true
opt.updatetime = 250
opt.timeout = true
opt.timeoutlen = 300

-- Search
opt.hlsearch = true
opt.incsearch = true
opt.ignorecase = false
opt.smartcase = true

-- Folding
opt.foldlevelstart = 10
opt.foldmethod = 'indent'
opt.foldenable = true

-- Display
opt.list = true
opt.listchars = { tab = '» ', trail = '·', nbsp = '␣' }
opt.fillchars = { eob = ' ' }

-- Completion
opt.completeopt = 'menuone,noselect'
opt.pumheight = 10

-- Performance
opt.lazyredraw = true
opt.synmaxcol = 240

-- Messages
opt.shortmess:append('c')
