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
	cmd('exec "!time python '..fullname..'"')
    elseif ext == "sh" then
	cmd('exec "!time sh '..fullname..'"')
    elseif ext == "hs" then
	cmd('exec "!time stack runhaskell '..fullname..'"')
	-- TODO: remove *.o and *.hi
    elseif ext == "c" then
	cmd('exec "!gcc '..fullname..' -o '..path..'/'..name..'"')
	cmd('exec "!time '..path..'/'..name..'"')
    elseif ext == "js" or ext == "ts" or ext == "tsx" then
    cmd('exec "!node '..path..'/'..name..'"')
    elseif ext == "tex" then
        cmd('exec "!xelatex  '..fullname..'"')
        cmd('silent exec "!rm '..path..'/*.{log,blg,bbl}"')
    elseif ext == "rkt" then
    elseif ext == "r" then
    elseif ext == "vrc" then
        cmd('exec "RunVrc"')
    elseif ext == "sql" then
        local file = io.open(fullname, "r")
        local fline = file:read()
        local dbname = M.Read_SQL_Config(fline, "dbname", "postgres")
        local line2 = file:read()
        local line3 = file:read()
        local line4 = file:read()
        local line5 = file:read()
        local host = M.Read_SQL_Config(line2, "host", "localhost")
        local port = M.Read_SQL_Config(line3, "port", "5432")
        local user = M.Read_SQL_Config(line4, "user", "postgres")
        local password = M.Read_SQL_Config(line5, "password", "postgres")
        local vimCmd = 'exec "!PGPASSWORD='..password..' psql -U '..user..' -p '..port..' -h '..host..' -d'..dbname..' < '..fullname..'"'
        cmd(vimCmd)
        file.close()
    end
end

function M.Read_SQL_Config(str, name, default)
    if str:find('^-- '..name) == nil then
        return default
    end
    return string.gsub(str, '-- '..name..' *', "")
end

return M
