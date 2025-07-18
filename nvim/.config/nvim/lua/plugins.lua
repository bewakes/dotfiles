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
                on_attach = function(bufnr)
                    local api = require('nvim-tree.api')
                    local opts = { buffer = bufnr, silent = true }

                    -- Default mappings
                    api.config.mappings.default_on_attach(bufnr)

                    -- Custom mappings
                    vim.keymap.set('n', 's', api.node.open.vertical, opts)
                    vim.keymap.set('n', 'i', api.node.open.horizontal, opts)
                    vim.keymap.set('n', 't', api.node.open.tab, opts)
                end,
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
    'nvim-treesitter/nvim-treesitter', -- syntax highlighting
    {
        'nvim-telescope/telescope.nvim',
        cmd = 'Telescope',
        keys = {
            { '<leader>s', ':Telescope lsp_document_symbols<CR>', desc = 'Document symbols' },
        },
        dependencies = {
            'nvim-lua/plenary.nvim',
            {
                'nvim-telescope/telescope-fzf-native.nvim',
                build = 'make',
                cond = function()
                    return vim.fn.executable 'make' == 1
                end,
            },
        },
        config = function()
            pcall(require('telescope').load_extension, 'fzf')
        end,
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
    { 'neovim/nvim-lspconfig' },
    { "williamboman/mason.nvim",           version = "^1.0.0" },
    { "williamboman/mason-lspconfig.nvim", version = "^1.0.0" },
    { "j-hui/fidget.nvim",                 opts = {} },

    -- Completion
    { 'hrsh7th/cmp-nvim-lsp' },
    { 'hrsh7th/nvim-cmp' },
    { 'L3MON4D3/LuaSnip' },
    'saadparwaiz1/cmp_luasnip',
    'rafamadriz/friendly-snippets',

    -- table
    { 'dhruvasagar/vim-table-mode' },

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

    -- Debug Adapter Protocol
    {
        'mfussenegger/nvim-dap',
        dependencies = {
            'rcarriga/nvim-dap-ui',
            'nvim-neotest/nvim-nio',
        },
        config = function()
            local dap = require('dap')
            local dapui = require('dapui')

            dapui.setup()

            -- Auto-open/close DAP UI
            dap.listeners.after.event_initialized['dapui_config'] = function()
                dapui.open()
            end
            dap.listeners.before.event_terminated['dapui_config'] = function()
                dapui.close()
            end
            dap.listeners.before.event_exited['dapui_config'] = function()
                dapui.close()
            end
        end,
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
        'folke/flash.nvim',
        event = 'VeryLazy',
        opts = {
            search = {
                multi_window = true,
                forward = true,
                wrap = true,
                mode = 'exact',
            },
            jump = {
                jumplist = true,
                pos = 'start',
                history = false,
            },
            label = {
                uppercase = true,
                exclude = '',
                current = true,
            },
            modes = {
                search = {
                    enabled = true,
                },
                char = {
                    enabled = true,
                    jump_labels = true,
                },
            },
        },
        keys = {
            { '<leader>x',  mode = { 'n', 'x', 'o' }, function() require('flash').jump() end,              desc = 'Flash Jump' },
            { '<leader>X',  mode = { 'n', 'x', 'o' }, function() require('flash').treesitter() end,        desc = 'Flash Treesitter' },
            { '<leader>xr', mode = 'o',               function() require('flash').remote() end,            desc = 'Remote Flash' },
            { '<leader>xR', mode = { 'o', 'x' },      function() require('flash').treesitter_search() end, desc = 'Treesitter Search' },
            { '<c-s>',      mode = { 'c' },           function() require('flash').toggle() end,            desc = 'Toggle Flash Search' },
        },
    },
    {
        'folke/trouble.nvim',
        cmd = { 'Trouble', 'TroubleToggle' },
        config = function()
            require('trouble').setup({
                icons = false,
                fold_open = 'v',
                fold_closed = '>',
                indent_lines = false,
                signs = {
                    error = 'E',
                    warning = 'W',
                    hint = 'H',
                    information = 'I',
                    other = 'O',
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
