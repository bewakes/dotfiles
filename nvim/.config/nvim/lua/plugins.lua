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
    'nvim-telescope/telescope-ui-select.nvim',

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
    -- haskell
    'alx741/vim-stylishask',
    'raichoo/haskell-vim',

    -- rust
    'rust-lang/rust.vim',
    'rouge8/neotest-rust',

    -- lua
    'tjdevries/nlua.nvim',

    -- Genai
    {
        "David-Kunz/gen.nvim",
        opts = {
            model = "mistral", -- The default model to use.
            quit_map = "q", -- set keymap to close the response window
            retry_map = "<c-r>", -- set keymap to re-send the current prompt
            accept_map = "<c-cr>", -- set keymap to replace the previous selection with the last result
            host = "localhost", -- The host running the Ollama service.
            port = "11434", -- The port on which the Ollama service is listening.
            display_mode = "float", -- The display mode. Can be "float" or "split" or "horizontal-split".
            show_prompt = false, -- Shows the prompt submitted to Ollama. Can be true (3 lines) or "full".
            show_model = true, -- Displays which model you are using at the beginning of your chat session.
            no_auto_close = false, -- Never closes the window automatically.
            file = false, -- Write the payload to a temporary file to keep the command short.
            hidden = false, -- Hide the generation window (if true, will implicitly set `prompt.replace = true`), requires Neovim >= 0.10
            init = function(options) pcall(io.popen, "ollama serve > /dev/null 2>&1 &") end,
            -- Function to initialize Ollama
            command = function(options)
                local body = {model = options.model, stream = true}
                return "curl --silent --no-buffer -X POST http://" .. options.host .. ":" .. options.port .. "/api/chat -d $body"
            end,
            -- The command for the Ollama service. You can use placeholders $prompt, $model and $body (shellescaped).
            -- This can also be a command string.
            -- The executed command must return a JSON object with { response, context }
            -- (context property is optional).
            -- list_models = '<omitted lua function>', -- Retrieves a list of model names
            result_filetype = "markdown", -- Configure filetype of the result buffer
            debug = false -- Prints errors and the command which is run.
        }
    }
});
