-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
	if vim.v.shell_error ~= 0 then
		vim.api.nvim_echo({
			{ "Failed to clone lazy.nvim:\n", "ErrorMsg" },
			{ out, "WarningMsg" },
			{ "\nPress any key to exit..." },
		}, true, {})
		vim.fn.getchar()
		os.exit(1)
	end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = ","
vim.g.maplocalleader = "\\"
-- spaces
vim.opt.expandtab = true
vim.opt.softtabstop = 4
vim.opt.tabstop = 4

-- Setup lazy.nvim
require("lazy").setup({
	spec = {
		-- add your plugins here
		{ "nvim-treesitter/nvim-treesitter", branch = "master", lazy = false, build = ":TSUpdate" },
		{
			"tpope/vim-repeat",
			config = function()
				-- vim-repeat requires no additional configuration
				-- It automatically enhances the . command for compatible plugins
			end,
		},
		"kylechui/nvim-surround",
		{
			"ibhagwan/fzf-lua",
			-- optional for icon support
			dependencies = { "nvim-tree/nvim-web-devicons" },
			-- or if using mini.icons/mini.nvim
			opts = {},
			keys = {
				{ "<leader>fv", "<cmd>FzfLua files<cr>", desc = "Fzf Files" },
				{ "<leader>fg", "<cmd>FzfLua live_grep<cr>", desc = "Fzf Grep" },
			},
		},
		{
			-- WINDOW PICKER
			"s1n7ax/nvim-window-picker",
			version = "v1.*",
			config = function()
				local picker = require("window-picker")
				picker.setup({ fg_color = "#000000" })

				vim.keymap.set("n", "<leader><leader>w", function()
					local picked_window_id = picker.pick_window() or vim.api.nvim_get_current_win()
					vim.api.nvim_set_current_win(picked_window_id)
				end, { desc = "Pick a window" })
			end,
		},
		{
			-- FORMATTING
			"stevearc/conform.nvim",
			config = function()
				local conform = require("conform")

				conform.setup({
					log_level = vim.log.levels.DEBUG, -- :ConformInfo to show log info
					formatters_by_ft = {
						-- https://www.gnu.org/software/gawk/manual/gawk.html
						awk = { "gawk" },
						-- https://github.com/mvdan/gofumpt
						-- https://pkg.go.dev/golang.org/x/tools/cmd/goimports (auto imports)
						-- https://github.com/incu6us/goimports-reviser
						go = { "gofumpt", "goimports", "goimports-reviser" },
						-- https://github.com/threedaymonk/htmlbeautifier
						html = { "htmlbeautifier" },
						-- https://github.com/mantoni/eslint_d.js/
						-- https://github.com/beautifier/js-beautify
						javascript = { "eslint_d", "js_beautify" },
						-- https://github.com/stedolan/jq
						jq = { "jq" },
						-- https://github.com/rhysd/fixjson
						json = { "fixjson" },
						-- https://github.com/JohnnyMorganz/StyLua
						lua = { "stylua" },
						-- https://github.com/executablebooks/mdformat
						markdown = { "mdformat" },
						-- https://github.com/rust-lang/rustfmt
						rust = { "rustfmt" },
						-- https://github.com/koalaman/shellcheck
						sh = { "shellcheck" },
						-- https://www.terraform.io/docs/cli/commands/fmt.html
						-- https://opentofu.org/docs/cli/commands/fmt/  NOTE: This is an alternative `tofu_fmt`
						terraform = { "terraform_fmt" },
						-- https://github.com/tamasfe/taplo
						toml = { "taplo" },
						-- http://xmlsoft.org/xmllint.html
						xml = { "xmllint" },
						-- https://github.com/mikefarah/yq
						yq = { "yq" },
						-- https://github.com/ziglang/zig
						zig = { "zigfmt" },
						zon = { "zigfmt" },
						-- https://github.com/koalaman/shellcheck
						zsh = { "shellcheck" },
					},
					format_after_save = function(bufnr)
						-- disable with a global or buffer-local variable
						if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
							return
						end
						return { timeout_ms = 5000, lsp_format = "fallback" }
					end,
				})

				vim.api.nvim_create_user_command("FormatDisable", function(args)
					if args.bang then
						-- FormatDisable! will disable formatting just for this buffer
						vim.b.disable_autoformat = true
					else
						vim.g.disable_autoformat = true
					end
				end, {
					desc = "Disable autoformat-on-save",
					bang = true,
				})

				vim.api.nvim_create_user_command("FormatEnable", function()
					vim.b.disable_autoformat = false
					vim.g.disable_autoformat = false
				end, {
					desc = "Re-enable autoformat-on-save",
				})

				vim.keymap.set("n", "<leader><leader>fi", "<Cmd>ConformInfo<CR>", { desc = "Show Conform log" })
				vim.keymap.set(
					"n",
					"<leader><leader>fd",
					"<Cmd>FormatDisable<CR>",
					{ desc = "Disable autoformat-on-save" }
				)
				vim.keymap.set(
					"n",
					"<leader><leader>fe",
					"<Cmd>FormatEnable<CR>",
					{ desc = "Re-enable autoformat-on-save" }
				)
				vim.keymap.set("n", "<leader><leader>fg", function()
					vim.cmd("silent !gofumpt -w %")
				end, { desc = "Format file with gofumpt" })

				-- DISABLED: in favour of format_on_save.
				--
				-- vim.api.nvim_create_autocmd("BufWritePre", {
				--   group = vim.api.nvim_create_augroup("Formatting", { clear = true }),
				--   pattern = "*",
				--   callback = function(args)
				--     require("conform").format({ bufnr = args.buf })
				--   end
				-- })
			end,
		},
		{
			-- LINTING
			"mfussenegger/nvim-lint",
			config = function()
				local lint = require("lint")

				lint.linters_by_ft = {
					-- https://www.gnu.org/software/gawk/
					awk = { "gawk" },
					-- https://github.com/codespell-project/codespell
					-- https://golangci-lint.run/
					go = { "codespell", "golangcilint" },
					-- https://htmlhint.com/
					-- https://www.html-tidy.org/
					html = { "tidy" },
					-- https://github.com/mantoni/eslint_d.js
					javascript = { "eslint_d" },
					-- https://github.com/zaach/jsonlint
					json = { "jsonlint" },
					-- https://github.com/mrtazz/checkmake
					make = { "checkmake" },
					-- https://alexjs.com/
					-- https://github.com/DavidAnson/markdownlint
					-- https://docs.getwoke.tech/
					markdown = { "alex", "markdownlint", "woke" },
					-- https://github.com/rust-lang/rust-clippy
					rust = { "clippy" },
					-- https://www.gnu.org/software/bash/
					-- https://www.shellcheck.net/
					sh = { "bash", "shellcheck" },
					-- https://github.com/terraform-linters/tflint
					-- https://github.com/aquasecurity/trivy (originally https://github.com/aquasecurity/tfsec)
					terraform = { "tflint", "trivy" },
					-- https://www.shellcheck.net/
					-- https://www.zsh.org/
					zsh = { "shellcheck", "zsh" },
				}

				-- WARNING: Removed luacheck linter due to problem with folke/neodev
				-- https://github.com/mpeterv/luacheck
				-- lua = { "luacheck" },

				-- Checkmake requires a ini file in the current directory
				-- Otherwise you have to specify a global one
				lint.linters.checkmake.args = {
					"--format='{{.LineNumber}}:{{.Rule}}:{{.Violation}}\n'",
					"--config",
					os.getenv("HOME") .. "/.config/checkmake.ini",
				}

				-- Spectral requires a ruleset in the current directory
				-- Otherwise you have to specify a global one
				lint.linters.spectral.args = {
					"lint",
					"-f",
					"json",
					"--ruleset",
					os.getenv("HOME") .. "/.spectral.yaml",
				}

				-- NOTE: We need custom logic for handling YAML linting.
				--
				-- https://github.com/rhysd/actionlint
				-- https://github.com/adrienverge/yamllint (https://yamllint.readthedocs.io/en/stable/rules.html)
				-- https://github.com/stoplightio/spectral (`npm install -g @stoplight/spectral-cli`)
				vim.api.nvim_create_autocmd({
					"BufReadPost",
					"BufWritePost",
					"InsertLeave",
				}, {
					group = vim.api.nvim_create_augroup("Linting", { clear = true }),
					callback = function(ev)
						-- print(string.format('event fired: %s', vim.inspect(ev)))
						-- print(vim.bo.filetype)
						if
							(string.find(ev.file, ".github/workflows/") or string.find(ev.file, ".github/actions/"))
							and vim.bo.filetype == "yaml"
						then
							lint.try_lint("actionlint")
						elseif vim.bo.filetype == "yaml" then
							local first_line = vim.fn.getline(1)
							if string.find(first_line, "openapi:") then
								lint.try_lint("spectral")
							else
								lint.try_lint("yamllint")
							end
						else
							lint.try_lint()
						end
					end,
				})
			end,
		},
	},
	-- Configure any other settings here. See the documentation for more details.
	-- colorscheme that will be used when installing plugins.
	install = { colorscheme = { "habamax" } },
	-- automatically check for plugin updates
	checker = { enabled = true },
})
