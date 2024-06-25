local M = {}
local cmd = vim.cmd

function M.create_augroup(autocmds, name)
    cmd('augroup ' .. name)
    cmd('autocmd!')
    for _, autocmd in ipairs(autocmds) do
	cmd('autocmd ' .. table.concat(autocmd, ' '))
    end
    cmd('augroup END')
end

M.Run = function ()
    local len = function(tbl)
      local getN = 0
      for n in pairs(tbl) do
        getN = getN + 1
      end
      return getN
    end
    local Split = function(s, delimiter)
         result = {};
         for match in (s..delimiter):gmatch("(.-)"..delimiter) do
             table.insert(result, match);
         end
         return result;
    end

    local SplitByDot = function(s)
	result = {};
	for match in s:gmatch("([^.]+)") do
	    table.insert(result, match)
	end
	return result;
    end

    local eval = vim.api.nvim_eval
    local fullname = eval('@%')
    local fullnameWoExt = SplitByDot(fullname)[1]
    local path = eval('expand("%:p:h")')
    local splitted = Split(fullname, "/") --eval('split("'..fullname..'", "/")')
    local filename = splitted[len(splitted)]
    local name_ext = SplitByDot(filename)
    local ext = name_ext[2]
    local name = name_ext[1]
    local fullpath = "'"..path.."/"..name.."'"

    if ext == "go" then
    cmd('exec "!time go run '..fullname..'"')
    elseif ext == "py" then
	cmd('exec "!time python3 '..fullname..'"')
    elseif ext == "sh" then
	cmd('exec "!time sh '..fullname..'"')
    elseif ext == "hs" then
	cmd('exec "!time stack runhaskell '..fullname..'"')
	-- TODO: remove *.o and *.hi
    elseif ext == "c" then
        cmd('exec "!gcc '..fullname..' -o '..path..'/'..name..'"')
        cmd('exec "!time '..path..'/'..name..'"')
    elseif ext == "js" or ext == "ts" or ext == "tsx" then
          cmd('exec "!node '..fullname..'"')
    elseif ext == "rs" then
        cmd('exec "!rustc '..fullname..' -o '..path..'/'..name..'"')
        cmd('exec "!time '..path..'/'..name..'"')
    elseif ext == "tex" then
        cmd('exec "!xelatex  '..fullname..'"')
        -- cmd('exec "!bibtex  '..fullnameWoExt..'"')
        -- cmd('exec "!xelatex  '..fullname..'"')
        -- cmd('exec "!xelatex  '..fullname..'"')
        -- cmd('silent exec "!rm '..path..'/*.{log,blg}"')
    elseif ext == "rkt" then
    elseif ext == "r" then
    elseif ext == "vrc" then
        cmd('exec "RunVrc"')
    elseif ext == "sql" then
        local file = io.open(fullname, "r")
        local fline = file:read()
        local command = string.gsub(fline, '%-%- ', '')
        local vimCmd = 'exec "!'..command..' < '..fullname..'"'
        cmd(vimCmd)
        file.close()
    end
end

return M
