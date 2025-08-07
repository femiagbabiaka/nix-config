vim.o.number = true
vim.o.relativenumber = false
vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.shiftwidth = 0
vim.o.smartindent = true
vim.o.expandtab = true
vim.o.swapfile = false
vim.o.undofile = true
vim.o.termguicolors = true
vim.o.incsearch = true
vim.o.signcolumn = "yes"
vim.g.mapleader = " "
vim.o.winborder = "rounded"

vim.keymap.set('n', '<leader>w', ':write<CR>')
vim.keymap.set('n', '<leader>q', ':quit<CR>')
vim.keymap.set('n', '<leader>lf', vim.lsp.buf.format)
vim.keymap.set('n', '<leader>li',
    function()
        vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
    end
)
vim.keymap.set('n', '<leader>pu', vim.pack.update)

vim.pack.add({
    { src = "https://github.com/stevearc/oil.nvim" },
    { src = 'https://github.com/nyoom-engineering/oxocarbon.nvim.git' },
    { src = 'https://github.com/neovim/nvim-lspconfig' },
    { src = 'https://github.com/echasnovski/mini.pick' },
    { src = 'https://github.com/echasnovski/mini.pairs' },
    { src = 'https://github.com/nvim-treesitter/nvim-treesitter.git' },
    {
        src = 'https://github.com/Saghen/blink.cmp',
        version = vim.version.range('1.x')
    },
})

vim.cmd.colorscheme "oxocarbon"
vim.opt.background = "dark"
-- make statusline transparent
vim.cmd(":hi statusline guibg=NONE")

-- mini.pick
require "mini.pick".setup()
vim.keymap.set('n', '<leader>f', ":Pick files<CR>")
vim.keymap.set('n', '<leader>b', ":Pick buffers<CR>")
vim.keymap.set('n', '<leader>h', ":Pick help<CR>")

-- mini.pairs
require "mini.pairs".setup()

-- oil
require "oil".setup()
vim.keymap.set('n', '<leader>e', ":Oil<CR>")

-- nvim-treesitter
require "nvim-treesitter.configs".setup({
    sync_install = true,
    ensure_installed = "all",
    highlight = { enable = true },
    ignore_install = { "ipkg" }
})

-- LSP
vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
-- blink.cmp
require "blink.cmp".setup({
    keymap = { preset = 'super-tab' }
})
-- servers
vim.lsp.enable(
    {
        "gopls",
        "helm_ls",
        "lua_ls",
        "nil_ls",
        "rubocop",
        "ruby_lsp",
        "rust_analyzer",
        "terraformls"
    }
)
vim.lsp.config("lua_ls", {
    settings = {
        Lua = {
            workspace = {
                library = vim.api.nvim_get_runtime_file("", true)
            }
        }
    }
})
