local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    'bewakes/vim-rest-client',

    -- utils
    {
        'nvim-tree/nvim-tree.lua',
        cmd = { 'NvimTreeToggle', 'NvimTreeOpen', 'NvimTreeFocus' },
        keys = {
            { '<leader>n', ':NvimTreeToggle<CR>', desc = 'Toggle file tree' },
        },
        dependencies = { 'nvim-tree/nvim-web-devicons' },
        config = function()
            require('nvim-tree').setup({
                view = {
                    width = 30,
                },
                renderer = {
                    group_empty = true,
                },
                filters = {
                    dotfiles = true,
                },
            })
        end,
    },
    'tpope/vim-fugitive',
    {
        'lewis6991/gitsigns.nvim',
        event = { 'BufReadPre', 'BufNewFile' },
        config = function()
            require('gitsigns').setup({
                signs = {
                    add = { text = '+' },
                    change = { text = '~' },
                    delete = { text = '_' },
                    topdelete = { text = '‾' },
                    changedelete = { text = '~' },
                },
            })
        end,
    },
    'justinmk/vim-dirvish', -- directory viewer
    'nvim-lua/plenary.nvim',
    'nvim-lualine/lualine.nvim',
    'nvim-treesitter/nvim-treesitter',  -- syntax highlighting
    {
        'nvim-telescope/telescope.nvim',
        cmd = 'Telescope',
        keys = {
            { '<leader>s', ':Telescope lsp_document_symbols<CR>', desc = 'Document symbols' },
        },
        dependencies = { 'nvim-lua/plenary.nvim' }
    },
    'nvim-telescope/telescope-ui-select.nvim',

    -- color schemes
    {
        'ellisonleao/gruvbox.nvim',
        lazy = false,
        priority = 1000,
    },
    { 
        'rose-pine/neovim', 
        name = 'rose-pine',
        lazy = false,
        priority = 1000,
    },

    -- LSP
    {'neovim/nvim-lspconfig'},
    {"williamboman/mason.nvim", version = "^1.0.0"},
    {"williamboman/mason-lspconfig.nvim", version = "^1.0.0"},
    { "j-hui/fidget.nvim", opts = {} },

    -- Completion
    {'hrsh7th/cmp-nvim-lsp'},
    {'hrsh7th/nvim-cmp'},
    {'L3MON4D3/LuaSnip'},
    'saadparwaiz1/cmp_luasnip',
    'rafamadriz/friendly-snippets',

    -- table
    {'dhruvasagar/vim-table-mode'},

    -- test
    {
      "nvim-neotest/neotest",
      dependencies = {
        "nvim-neotest/nvim-nio",
        "nvim-lua/plenary.nvim",
        "antoinemadec/FixCursorHold.nvim",
        "nvim-treesitter/nvim-treesitter"
      }
    },
    -- haskell
    'alx741/vim-stylishask',
    'raichoo/haskell-vim',

    -- rust
    'rust-lang/rust.vim',
    'rouge8/neotest-rust',

    -- lua
    'tjdevries/nlua.nvim',

    -- UI enhancements
    {
        'folke/which-key.nvim',
        event = 'VeryLazy',
        config = function()
            require('which-key').setup({
                plugins = {
                    marks = true,
                    registers = true,
                    presets = {
                        operators = true,
                        motions = true,
                        text_objects = true,
                        windows = true,
                        nav = true,
                        z = true,
                        g = true,
                    },
                },
            })
        end,
    },
    {
        'windwp/nvim-autopairs',
        event = 'InsertEnter',
        config = function()
            require('nvim-autopairs').setup({
                check_ts = true,
                ts_config = {
                    lua = { 'string', 'source' },
                    javascript = { 'string', 'template_string' },
                    java = false,
                },
            })
        end,
    },
    {
        'lukas-reineke/indent-blankline.nvim',
        main = 'ibl',
        config = function()
            require('ibl').setup({
                indent = {
                    char = '│',
                    tab_char = '│',
                },
                scope = {
                    enabled = true,
                    show_start = true,
                    show_end = true,
                },
            })
        end,
    },
    {
        'folke/persistence.nvim',
        event = 'BufReadPre',
        config = function()
            require('persistence').setup({
                dir = vim.fn.expand(vim.fn.stdpath('state') .. '/sessions/'),
                options = { 'buffers', 'curdir', 'tabpages', 'winsize' },
            })
        end,
    },

});
