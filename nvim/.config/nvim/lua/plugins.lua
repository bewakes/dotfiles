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
    -- 'tpope/vim-fugitive',
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
    'lifepillar/vim-solarized8',

    -- lsp clients
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
    -- 'neovim/nvim-lsp',
    'neovim/nvim-lspconfig',
    --
    -- DAP
    -- 'mfussenegger/nvim-dap',
    -- 'rcarriga/nvim-dap-ui',

    -- completion
    -- 'hrsh7th/nvim-cmp',
    -- 'nvim-lua/completion-nvim',

    -- haskell
    -- 'alx741/vim-stylishask',
    -- 'raichoo/haskell-vim',

    -- rust
    'rust-lang/rust.vim',
    'simrat39/rust-tools.nvim',

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
