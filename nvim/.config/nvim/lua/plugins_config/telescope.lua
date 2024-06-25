require'telescope'.setup{
    defaults = {
        mappings = {
            i = {
                ["<c-k>"] = require("telescope.actions").move_selection_previous,
                ["<c-j>"] = require("telescope.actions").move_selection_next,
            }
        }
    },
    extensions = {
        ["ui-select"] = {
            require("telescope.themes").get_dropdown {
              -- even more opts
            }
        }
    }
}

require("telescope").load_extension("ui-select")

local builtin = require('telescope.builtin')

vim.keymap.set('n', '<c-p>', builtin.find_files, {})
vim.keymap.set('n', '<c-b>', builtin.buffers, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
