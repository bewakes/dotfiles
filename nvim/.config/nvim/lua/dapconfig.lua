require("dapui").setup()
local dap, dapui = require("dap"), require("dapui")


dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open()
end

dap.listeners.before.event_terminated["dapui_config"] = function()
    dapui.close()
end

dap.listeners.before.event_exited["dapui_config"] = function()
    dapui.open()
end

dap.adapters.lldb = {
  type = 'executable',
  command = '/opt/homebrew/opt/llvm/bin/lldb-vscode',
  name = "lldb",
}

vim.keymap.set('n', '<leader>duo', dapui.open, {})
vim.keymap.set('n', '<leader>dux', dapui.close, {})


dap.configurations.rust = {
    {
        type = 'lldb';
        request = 'launch';
        name = "Project";
        program = function()
          return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
        end,
        cwd = '${workspaceFolder}',
        stopOnEntry = false,
        args = {},
        runInTerminal = false,
    }
}
