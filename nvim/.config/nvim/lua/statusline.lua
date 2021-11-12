-- Modified from https://icyphox.sh/blog/nvim-lua/

local mode_map = {
    ['n'] = 'NORMAL',
    ['no'] = 'n·operator pending',
    ['v'] = 'VISUAL',
    ['V'] = 'V·LINE',
    [''] = 'V·BLOCK',
    ['s'] = 'SELECT',
    ['S'] = 's·line',
    [''] = 's·block',
    ['i'] = 'INSERT',
    ['R'] = 'REPLACE',
    ['Rv'] = 'v·replace',
    ['c'] = 'COMMAND',
    ['cv'] = 'vim ex',
    ['ce'] = 'ex',
    ['r'] = 'prompt',
    ['rm'] = 'more',
    ['r?'] = 'confirm',
    ['!'] = 'shell',
    ['t'] = 'TERMINAL'
}

local function mode()
    local m = vim.api.nvim_get_mode().mode
    if mode_map[m] == nil then return m end
    return mode_map[m]
end

vim.api.nvim_exec(
[[
    hi link InsertModeBlock InsertMode
    hi ModeBlock guibg=#859900 guifg=white
    hi link FileBlock StatusLineNC
    hi link SecondaryBlock Question
    hi link Blanks StatusLine
    hi link FilePercentage DiffChange
]], false)

local handle = io.popen('git branch | grep \\* | cut -d\' \' -f 2')
local branch = handle:read("*a")
branch = branch:gsub("%s+$", "") -- remove trailing whitespacs
handle:close()


local stl = function()
    local m = mode()
    local modeblock = ((m == "INSERT") and 'InsertModeBlock') or 'ModeBlock'
    local branchinfo = '═╣'..branch..' '
    if (branch == '') then
        branchinfo = ''
    end
    return {
        '%#'..modeblock..'#',
        ' '..mode()..' ',
        branchinfo,
        '%#Blanks#',
        ' %f',
        '%m',
        '%=',
        '%#SecondaryBlock#',
        ' %l,%c ',
        '%#FileBlock#',
        ' %{&filetype} ',
        '%#FilePercentage#',
        ' %P ',
    }
end

Statusline = function ()
    return table.concat(stl())
end

vim.api.nvim_exec(
[[
augroup StatusLine
au!
au WinEnter,BufEnter * setlocal statusline=%!v:lua.Statusline()
augroup END
]], false)
