vim.api.nvim_exec([[
autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()
]], false)
vim.api.nvim_exec([[
autocmd FileType go setlocal noet ci pi sts=0 sw=4 ts=4
]], false)
vim.api.nvim_exec([[
autocmd FileType go set nolist
]], false)

-- Template for react/react native
vim.api.nvim_exec([[
autocmd BufNewFile *.tsx
    \ call append("%", [
    \           "import React from 'react';",
    \           "import {View} from 'react-native';",
    \           "",
    \           "interface ComponentProps {",
    \           "}",
    \           "",
    \           "const Component: React.FC<ComponentProps> = (props) => {",
    \           "};",
    \           "",
    \           "export default Component;",
    \ ]) |
    \ %s/Component/\=expand("%:t:r")/g
]], false)
