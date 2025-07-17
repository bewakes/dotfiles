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
    'tpope/vim-fugitive',
    'justinmk/vim-dirvish', -- directory viewer
    'nvim-lua/plenary.nvim',
    'nvim-lualine/lualine.nvim',
    'nvim-treesitter/nvim-treesitter',  -- syntax highlighting
    {
        'nvim-telescope/telescope.nvim',
        requires = { { 'nvim-lua/plenary.nvim'} }
    },
    'nvim-telescope/telescope-ui-select.nvim',

    -- color schemes
    'ellisonleao/gruvbox.nvim',
    { 'rose-pine/neovim', name = 'rose-pine' },

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

});
