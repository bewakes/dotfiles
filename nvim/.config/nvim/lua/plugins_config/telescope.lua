require'telescope'.setup{
    defaults = {
        mappings = {
            i = {
                ["<c-k>"] = require("telescope.actions").move_selection_previous,
                ["<c-j>"] = require("telescope.actions").move_selection_next,
                ["<c-f>"] = require("telescope.actions").to_fuzzy_refine,
            },
            n = {
                ["<leader>x"] = function(prompt_bufnr)
                    require("flash").jump({
                        pattern = "^",
                        label = { after = { 0, 0 } },
                        search = {
                            mode = "search",
                            exclude = {
                                function(win)
                                    return vim.bo[vim.api.nvim_win_get_buf(win)].filetype ~= "TelescopeResults"
                                end,
                            },
                        },
                        action = function(match)
                            local picker = require("telescope.actions.state").get_current_picker(prompt_bufnr)
                            picker:set_selection(match.pos[1] - 1)
                        end,
                    })
                end,
            },
        },
        vimgrep_arguments = {
            'rg',
            '--color=never',
            '--no-heading',
            '--with-filename',
            '--line-number',
            '--column',
            '--smart-case',
            '--hidden',
            '--glob=!.git/',
        },
        file_ignore_patterns = {
            "node_modules",
            ".git/",
            "target/",
            "build/",
            "dist/",
            "*.min.js",
            "*.min.css",
            ".DS_Store",
            "*.log",
            "*.tmp",
            "*.lock",
        },
        layout_strategy = 'horizontal',
        layout_config = {
            horizontal = {
                prompt_position = 'top',
                preview_width = 0.55,
            },
            vertical = {
                mirror = false,
            },
            width = 0.87,
            height = 0.80,
            preview_cutoff = 120,
        },
        sorting_strategy = 'ascending',
        prompt_prefix = '🔍 ',
        selection_caret = '➤ ',
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

vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
vim.keymap.set('n', '<c-b>', builtin.buffers, {})
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})

-- Enhanced search with file filtering
vim.keymap.set('n', '<leader>fs', function()
    builtin.grep_string({ search = vim.fn.input("Grep > ") })
end, { desc = "Search for string with file filtering" })

vim.keymap.set('n', '<leader>fw', function()
    builtin.grep_string({ search = vim.fn.expand("<cword>") })
end, { desc = "Search for word under cursor" })

vim.keymap.set('n', '<leader>fG', function()
    builtin.live_grep({
        additional_args = function() return {"--hidden"} end
    })
end, { desc = "Live grep including hidden files" })

vim.keymap.set('n', '<leader>ft', function()
    builtin.live_grep({
        type_filter = vim.fn.input("File type (e.g., lua, py, js): ")
    })
end, { desc = "Live grep filtered by file type" })
