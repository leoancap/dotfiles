-- Install packer
local fn = vim.fn
local packer_bootstrap
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap =
    fn.system({"git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path})
end

vim.cmd [[
  augroup Packer
    autocmd!
    autocmd BufWritePost init.lua PackerCompile
  augroup end
]]

local lsp = require "lspconfig"
local packer = require "packer"

packer.startup(
  function(use)
    use "wbthomason/packer.nvim"
    use {
      "tpope/vim-fugitive", -- Git commands in nvim
      "tpope/vim-rhubarb", -- Fugitive-companion to interact with github
      "tpope/vim-commentary", -- "gc" to comment visual regions/lines
      "JoosepAlviste/nvim-ts-context-commentstring",
      "tpope/vim-surround",
      "tpope/vim-sensible",
      "famiu/bufdelete.nvim",
      "windwp/nvim-autopairs",
      "andymass/vim-matchup",
      "alvan/vim-closetag",
      "yuttie/comfortable-motion.vim",
      "mattn/emmet-vim",
      "mg979/vim-visual-multi",
      "christoomey/vim-tmux-navigator",
      "sbdchd/neoformat"
    }
    use {
      "kyazdani42/nvim-web-devicons",
      "kyazdani42/nvim-tree.lua",
      "akinsho/bufferline.nvim",
      "nvim-lualine/lualine.nvim",
      "lukas-reineke/indent-blankline.nvim",
      "airblade/vim-gitgutter",
      "nvim-telescope/telescope.nvim",
      "nvim-lua/plenary.nvim"
    }
    use {
      "joshdick/onedark.vim",
      "mhartington/oceanic-next",
      "gruvbox-community/gruvbox",
      "doums/darcula",
      "rebelot/kanagawa.nvim"
    }
    use {
      "jparise/vim-graphql",
      "styled-components/vim-styled-components",
      "nkrkv/nvim-treesitter-rescript",
      "rescript-lang/vim-rescript",
      "nvim-treesitter/nvim-treesitter-textobjects",
      "neovim/nvim-lspconfig",
      "hrsh7th/nvim-cmp" -- Autocompletion plugin
    }
    use {"nvim-treesitter/nvim-treesitter", run = ":TSUpdate"}
    -- Additional textobjects for treesitter
    use {
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-nvim-lsp",
      "saadparwaiz1/cmp_luasnip",
      "L3MON4D3/LuaSnip" -- Snippets plugin
    }

    if packer_bootstrap then
      require("packer").sync()
    end
  end
)

local opt = vim.opt
local g = vim.g
--
opt.ruler = false
opt.hidden = true
opt.ignorecase = true
opt.number = true
opt.numberwidth = 2
opt.relativenumber = false
opt.expandtab = true
opt.shiftwidth = 2
opt.smartindent = true
opt.splitbelow = true
opt.splitright = true
opt.termguicolors = true
opt.cul = true
opt.completeopt = {"menu", "menuone", "noselect", "preview"}
opt.clipboard = "unnamedplus"
opt.mouse = "a"
opt.undofile = true
opt.cmdheight = 1
opt.updatetime = 250
opt.timeoutlen = 400
opt.signcolumn = "yes"
g.mapleader = ","
g.auto_save = false
vim.cmd [[syntax on]]
vim.cmd [[set formatoptions-=cro]]
vim.cmd [[set nowrap]]
vim.cmd "set omnifunc=rescript#Complete"
-- vim.cmd "colorscheme kanagawa"
vim.cmd "colorscheme gruvbox"
vim.cmd [[
  " backup to ~/.tmp 
  set noswapfile
  set backup 
  set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp 
  set backupskip=/tmp/*,/private/tmp/* 
  set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp 
  set writebackup
]]

require "lualine".setup {
  options = {
    theme = "auto"
  },
  sections = {
    lualine_c = {"path", "filename"}
  }
}

-- Mappings
local keyOpts = {noremap = true, silent = true}

local function remap(mode, mapping, cmd, options)
  vim.api.nvim_set_keymap(mode, mapping, cmd, options)
end

vim.cmd [[
    vnoremap <S-J> :m '>+1<CR>gv=gv
    vnoremap <S-K> :m '<-2<CR>gv=gv
]]

-- Telescope
remap("n", "<leader>t<space>", ":Telescope buffers<CR>", keyOpts)
remap("n", "<C-p>", [[<cmd>lua require('telescope.builtin').find_files({previewer = false})<CR>]], keyOpts)
remap("n", "<leader>tp", ":Telescope find_files<CR>", keyOpts)
remap("n", "<leader>tl", ":Telescope live_grep<CR>", keyOpts)
remap("n", "<leader>tf", ":Telescope current_buffer_fuzzy_find<CR>", keyOpts)
remap("n", "<leader>th", ":Telescope help_tags<CR>", keyOpts)
remap("n", "<leader>tb", ":Telescope git_branches<CR>", keyOpts)
remap("n", "<leader>tg", ":Telescope git_files<CR>", keyOpts)
remap("n", "<leader>td", ":Telescope grep_string<CR>", keyOpts)
remap("n", "<leader>to", ":Telescope oldfiles<CR>", keyOpts)
remap("n", "<leader>tc", [[<cmd>lua require('telescope.builtin').tags{ only_current_buffer = true }<CR>]], keyOpts)

remap("n", "<leader>n", [[:NvimTreeToggle<CR>]], keyOpts)
remap("n", "<C-s>", ":w<CR>", keyOpts)
remap("i", "<C-s>", [[<Esc>:w!<CR>]], keyOpts)
remap("n", "<C-w>", [[:Bdelete<CR>]], keyOpts)
remap("i", "<C-w>", [[:Bdelete<CR>]], keyOpts)
remap("n", "gt", [[:BufferLineCycleNext<CR>]], keyOpts)
remap("n", "gT", [[:BufferLineCyclePrev<CR>]], keyOpts)
remap("n", "<leader>f", [[:BufferLinePick<CR>]], keyOpts)
remap("n", "<leader>1", [[:BufferLineGoToBuffer 1<CR>]], keyOpts)
remap("n", "<leader>2", [[:BufferLineGoToBuffer 2<CR>]], keyOpts)
remap("n", "<leader>3", [[:BufferLineGoToBuffer 3<CR>]], keyOpts)
remap("n", "<leader>4", [[:BufferLineGoToBuffer 4<CR>]], keyOpts)
remap("n", "Y", "y$", {noremap = true})
remap("n", "<leader><leader>1", [[:source ~/.config/nvim/init.lua<CR>]], keyOpts)
remap("n", "<leader><leader>2", [[:source ~/.config/nvim/init.lua <Bar> :PackerInstall<CR>]], keyOpts)
--remap for dealing with word wrap
remap("n", "k", "v:count == 0 ? 'gk' : 'k'", {noremap = true, expr = true, silent = true})
remap("n", "j", "v:count == 0 ? 'gj' : 'j'", {noremap = true, expr = true, silent = true})

-- Highlight on yank
vim.cmd [[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]]

--Map blankline
-- vim.g.indent_blankline_filetype_exclude = {"help", "packer"}
-- vim.g.indent_blankline_buftype_exclude = {"terminal", "nofile"}
-- vim.g.indent_blankline_char_highlight = "LineNr"
-- vim.g.indent_blankline_show_trailing_blankline_indent = false

require("bufferline").setup {
  options = {
    separator_style = "slant",
    diagnostics = "nvim_lsp",
    diagnostics_indicator = function(count, level, diagnostics_dict, context)
      local s = level .. " "
      for e, n in pairs(diagnostics_dict) do
        local sym = e == "error" and " " or (e == "warning" and " " or "")
        s = s .. n .. sym
      end
      return s
    end,
    offsets = {
      {
        filetype = "NvimTree",
        text = "File Explorer",
        highlight = "Directory",
        text_align = "left"
      }
    }
  }
}

-- AutoClose tag
g.closetag_filenames = "*.html,*.tsx"

-- Emmet
g.user_emmet_leader_key = "<C-e>"
vim.cmd "au FileType html,css,jsx,rescript EmmetInstall"

require("nvim-autopairs").setup {}

-- Nvim-tree
vim.g.nvim_tree_quit_on_open = 1
vim.g.nvim_tree_indent_markers = 1

require "nvim-tree".setup {
  open_on_setup = false,
  auto_close = false,
  update_focused_file = {enable = true},
  filters = {
    dotfiles = true,
    custom = {"**/*.bs.js"}
  },
  view = {
    allow_resize = true,
    width = 30,
    side = "left"
  }
}

-- Telescope
require("telescope").setup {
  defaults = {
    mappings = {}
  }
}

-- Treesitter configuration
-- Parsers must be installed manually via :TSInstall
require("nvim-treesitter.configs").setup {
  highlight = {
    enable = true, -- false will disable the whole extension
    disalbe = {}
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm"
    }
  },
  indent = {
    enable = false,
    disable = {}
  },
  ensure_installed = {
    "tsx",
    "javascript",
    "typescript",
    "prisma",
    "rescript",
    "graphql",
    "json",
    "scss",
    "html",
    "yaml",
    "lua"
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner"
      }
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = "@class.outer"
      },
      goto_next_end = {
        ["]M"] = "@function.outer",
        ["]["] = "@class.outer"
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer"
      },
      goto_previous_end = {
        ["[M"] = "@function.outer",
        ["[]"] = "@class.outer"
      }
    }
  },
  context_commentstring = {
    enable = true
  }
}

-- LSP settings
local nvim_lsp = require "lspconfig"
local on_attach = function(_, bufnr)
  local function bufMap(buffer, mode, mapping, cmd, options)
    vim.api.nvim_buf_set_keymap(buffer, mode, mapping, cmd, options)
  end
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  remap("n", "<leader>tr", [[<cmd>lua require('telescope.builtin').lsp_references()<CR>]], keyOpts)
  remap("n", "<leader>ta", [[<cmd>lua require('telescope.builtin').lsp_code_actions()<CR>]], keyOpts)

  bufMap(bufnr, "n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", keyOpts)
  bufMap(bufnr, "n", "<leader>gr", "<cmd>lua vim.lsp.buf.rename()<CR>", keyOpts)
  bufMap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", keyOpts)
  bufMap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", keyOpts)
  bufMap(bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", keyOpts)
  bufMap(bufnr, "n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", keyOpts)
  bufMap(bufnr, "n", "<leader>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", keyOpts)
  bufMap(bufnr, "n", "<leader>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", keyOpts)
  bufMap(bufnr, "n", "<leader>wl", "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", keyOpts)
  bufMap(bufnr, "n", "<leader>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", keyOpts)
  bufMap(bufnr, "n", "<leader>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", keyOpts)
  bufMap(bufnr, "n", "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", keyOpts)
  bufMap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", keyOpts)
  bufMap(bufnr, "n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", keyOpts)
  bufMap(bufnr, "v", "<leader>ca", "<cmd>lua vim.lsp.buf.range_code_action()<CR>", keyOpts)
  bufMap(bufnr, "n", "<leader>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", keyOpts)
  bufMap(bufnr, "n", "<leader>[", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", keyOpts)
  bufMap(bufnr, "n", "<leader>]", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", keyOpts)
  bufMap(bufnr, "n", "<leader>q", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", keyOpts)
  bufMap(bufnr, "n", "<leader>so", [[<cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>]], keyOpts)
  vim.cmd [[ command! Format execute 'lua vim.lsp.buf.formatting()' ]]
end

-- nvim-cmp supports additional completion capabilities
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

-- Enable the following language servers

nvim_lsp.tsserver.setup {
  on_attach = function(client, bufnr)
    client.resolved_capabilities.document_formatting = false
    client.resolved_capabilities.document_range_formatting = false
    on_attach(client, bufnr)
  end,
  capabilities = capabilities
}

lsp.graphql.setup {
  cmd = {"graphql-lsp", "server", "-m", "stream"},
  filetypes = {"graphql", "typescriptreact", "javascriptreact", "rescript"}
}
lsp.prismals.setup {
  cmd = {"prisma-language-server", "--stdio"},
  filetypes = {"prisma"},
  settings = {
    prisma = {
      prismaFmtBinPath = ""
    }
  }
}

local rescriptLspPath = "/home/leo/.local/share/nvim/site/pack/packer/start/vim-rescript/server/out/server.js"
lsp.rescriptls.setup {
  cmd = {
    "node",
    rescriptLspPath,
    "--stdio"
  },
  on_attach = on_attach,
  capabilities = capabilities
}

USER = fn.expand("$USER")
local sumneko_root_path = ""
local sumneko_binary = ""

if fn.has("unix") == 1 then
  sumneko_root_path = "/usr/bin/lua-language-server"
  sumneko_binary = "/usr/bin/lua-language-server"
else
  print("Unsupported system for sumneko")
end

lsp.sumneko_lua.setup {
  cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"},
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = "LuaJIT",
        -- Setup your lua path
        path = vim.split(package.path, ";")
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {"vim"}
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = {[fn.expand("$VIMRUNTIME/lua")] = true, [fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true}
      }
    }
  }
}

g.neoformat_lua_customLuafmt = {
  exe = "luafmt",
  args = {"--indent-count", 2, "--stdin"},
  stdin = 1
}
g.neoformat_enabled_lua = {"customLuafmt"}
g.neoformat_only_msg_on_error = 1

vim.api.nvim_exec(
  [[
  augroup fmt
    autocmd!
    autocmd BufWritePre *.lua  undojoin | Neoformat
    autocmd BufWritePre *.ts* undojoin | Neoformat prettier
    autocmd BufWritePre *.js Neoformat eslint_d
    autocmd BufWritePre *.json Neoformat prettier
    autocmd BufWritePre *.res lua vim.lsp.buf.formatting_sync()
    autocmd BufWritePre *.prisma lua vim.lsp.buf.formatting_sync()
  augroup END
]],
  true
)

-- luasnip setup
local luasnip = require "luasnip"

-- nvim-cmp setup
local cmp = require "cmp"
cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end
  },
  experimental = {ghost_text = true, native_menu = false},
  mapping = {
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.close(),
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true
    },
    ["<Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end,
    ["<S-Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end
  },
  sources = {
    {name = "nvim_lsp"},
    {name = "luasnip"},
    {name = "buffer"},
    {name = "path"}
  }
}
