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
    'scrooloose/nerdtree', -- project viewer
    -- 'w0rp/ale', -- linter, autocomplete
    'tpope/vim-fugitive',
    -- 'airblade/vim-gitgutter', -- git diffs
    'justinmk/vim-dirvish', -- directory viewer
    'nvim-lua/plenary.nvim',
    'nvim-lualine/lualine.nvim',
    'nvim-treesitter/nvim-treesitter',  -- syntax highlighting
    {
        'nvim-telescope/telescope.nvim',
        requires = { { 'nvim-lua/plenary.nvim'} }
    },

    -- color schemes
    'ellisonleao/gruvbox.nvim',
    { 'rose-pine/neovim', name = 'rose-pine' },
    -- 'lifepillar/vim-solarized8',

    -- LSP
    {'neovim/nvim-lspconfig'},
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    { "j-hui/fidget.nvim", opts = {} },
    --
    -- DAP
    'mfussenegger/nvim-dap',
    'rcarriga/nvim-dap-ui',

    -- Completion
    {'hrsh7th/cmp-nvim-lsp'},
    {'hrsh7th/nvim-cmp'},
    {'L3MON4D3/LuaSnip'},
    'saadparwaiz1/cmp_luasnip',
    'rafamadriz/friendly-snippets',

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
    'rouge8/neotest-rust',

    -- haskell
    -- 'alx741/vim-stylishask',
    -- 'raichoo/haskell-vim',

    -- rust
    'rust-lang/rust.vim',
    -- 'simrat39/rust-tools.nvim',

    -- js/ts
    'maxmellon/vim-jsx-pretty',

    -- python

    -- go
    -- 'fatih/vim-go',

    -- lua
    'tjdevries/nlua.nvim',

    -- purescript
    -- 'purescript-contrib/purescript-vim',

    -- reload
    -- 'famiu/nvim-reload',

    -- null-ls
    -- 'jose-elias-alvarez/null-ls.nvim',
});
