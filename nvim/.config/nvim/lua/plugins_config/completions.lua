local cmp = require("cmp")
require("luasnip.loaders.from_vscode").lazy_load()

cmp.setup({
    window = {
        completion = cmp.config.window.bordered({
            border = 'rounded',
            winhighlight = 'Normal:Normal,FloatBorder:FloatBorder,CursorLine:Visual,Search:None',
        }),
        documentation = cmp.config.window.bordered({
            border = 'rounded',
            winhighlight = 'Normal:Normal,FloatBorder:FloatBorder,CursorLine:Visual,Search:None',
        }),
    },
    mapping = cmp.mapping.preset.insert({
        ['<C-o>'] = cmp.mapping.complete(),
        ['<CR>'] = cmp.mapping.confirm({select=true}),
    }),
    snippet = {
        expand = function(args)
            require("luasnip").lsp_expand(args.body)
        end
    },
    sources = cmp.config.sources({
        {name = 'nvim_lsp'},
        {name = 'luasnip'}
    }, {
        {name = 'buffer'}
    })
})
