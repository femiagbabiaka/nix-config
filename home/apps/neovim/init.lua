vim.o.number = true
vim.o.relativenumber = true
vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.expandtab = true
vim.o.swapfile = false
vim.g.mapleader = " "
vim.o.winborder = "rounded"

vim.keymap.set('n', '<leader>w', ':write<CR>')
vim.keymap.set('n', '<leader>q', ':quit<CR>')
vim.keymap.set('n', '<leader>lf', vim.lsp.buf.format)

vim.pack.add({
        { src = 'https://github.com/nyoom-engineering/oxocarbon.nvim.git' },
        { src = 'https://github.com/neovim/nvim-lspconfig' },
        { src = 'https://github.com/echasnovski/mini.pick' },
})

vim.cmd.colorscheme "oxocarbon"
vim.opt.background = "light"
-- make statusline transparent
vim.cmd(":hi statusline guibg=NONE")

-- mini.pick
require "mini.pick".setup()
vim.keymap.set('n', '<leader>f', ":Pick files<CR>")
vim.keymap.set('n', '<leader>b', ":Pick buffers<CR>")
vim.keymap.set('n', '<leader>v', ":Pick files tool=git<CR>")


-- LSP
-- autocomplete
vim.api.nvim_create_autocmd('LspAttach', {
        callback = function(ev)
                local client = vim.lsp.get_client_by_id(ev.data.client_id)
                if client:supports_method('textDocument/completion') then
                        vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = true })
                end
        end,
})
vim.cmd("set completeopt +=noselect")
-- servers
vim.lsp.enable(
        {
                "lua_ls",
                "nil_ls",
                "gopls"
        }
)
