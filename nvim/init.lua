-- ============================================================================
-- Neovim Configuration - Modular Single File
-- ============================================================================

local eink = dofile(vim.fn.expand("~/dotfiles/nvim/eink.lua"))
eink.set_eink_theme()

-- ============================================================================
-- Module: Options
-- ============================================================================
local Options = {}

function Options.setup()
  local opt = vim.opt
  local g = vim.g

  -- Leader key
   g.mapleader = ","

  -- UI Settings
  opt.title = true
  opt.titlestring = "%t%m%r - nvim"
  opt.number = true
  opt.numberwidth = 2
  opt.relativenumber = false
  opt.ruler = false
  opt.showmode = false
  opt.signcolumn = "yes"
  opt.cursorline = true
  opt.termguicolors = true
  opt.cmdheight = 0
  opt.laststatus = 0

  -- Editor behavior
  opt.hidden = true
  opt.mouse = "a"
  opt.clipboard = "unnamedplus"
  opt.undofile = true
  opt.autoread = true
  opt.wrap = false

  -- Search
  opt.ignorecase = true

  -- Indentation
  opt.expandtab = true
  opt.shiftwidth = 2
  opt.tabstop = 2
  opt.softtabstop = 2
  opt.smartindent = true
  opt.autoindent = true
  opt.cindent = true

  -- Splits
  opt.splitbelow = true
  opt.splitright = true

  -- Completion
  opt.completeopt = {"menu", "menuone", "noselect", "preview"}

  -- Timing
  opt.updatetime = 251
  opt.timeoutlen = 401

  -- Background
  opt.background = "light"

  -- Backup settings
  opt.swapfile = false
  opt.backup = true
  opt.backupdir = vim.fn.expand("~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp")
  opt.backupskip = "/tmp/*,/private/tmp/*"
  opt.directory = vim.fn.expand("~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp")
  opt.writebackup = true

  -- Misc
  g.auto_save = false
  opt.formatoptions:remove({ "c", "r", "o" })

  -- Filetype settings
  vim.cmd("filetype indent off")
end

-- ============================================================================
-- Module: Theme
-- ============================================================================
local Theme = {}

function Theme.set_light_theme()
  eink.set_eink_theme()
end

function Theme.set_dark_theme()
  vim.opt.background = "dark"
end

function Theme.setup()
  -- Apply light theme by default
  Theme.set_light_theme()
end

-- ============================================================================
-- Module: Autocmds
-- ============================================================================
local Autocmds = {}

function Autocmds.setup()
  -- Auto reload files on focus/buffer change
  local aug = vim.api.nvim_create_augroup("AutoReload", { clear = true })
  
  vim.api.nvim_create_autocmd("FocusGained", {
    desc = "Reload files from disk when we focus vim",
    pattern = "*",
    group = aug,
    callback = function()
      if vim.fn.getcmdwintype() == '' then
        vim.cmd("checktime")
      end
    end,
  })

  vim.api.nvim_create_autocmd("BufEnter", {
    desc = "Every time we enter an unmodified buffer, check if it changed on disk",
    pattern = "*",
    group = aug,
    callback = function()
      if vim.bo.buftype == '' and not vim.bo.modified and vim.fn.expand('%') ~= '' then
        vim.cmd("checktime " .. vim.fn.expand('<abuf>'))
      end
    end,
  })

  -- Highlight on yank
  vim.api.nvim_create_autocmd("TextYankPost", {
    desc = "Highlight text on yank",
    group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
    callback = function()
      vim.highlight.on_yank()
    end,
  })

  -- File type specific settings
  vim.api.nvim_create_autocmd({"BufNewFile", "BufRead", "BufReadPost"}, {
    pattern = "*.re",
    callback = function()
      vim.bo.filetype = "reason"
      vim.bo.syntax = "ocaml"
    end,
  })

  -- Emmet for specific file types
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "html,css,jsx,rescript,reason",
    command = "EmmetInstall",
  })

  -- Tame OCaml/Reason type annotation highlights
  local function tame_ocaml_hl()
    vim.api.nvim_set_hl(0, "ocamlTypeCatchAll", { link = "Type" })
    vim.api.nvim_set_hl(0, "ocamlTypeAnnot", { link = "Type" })
  end
  vim.api.nvim_create_autocmd({ "FileType", "Syntax" }, {
    pattern = { "ocaml", "reason", "rescript" },
    callback = function()
      vim.defer_fn(tame_ocaml_hl, 1)
    end,
  })
  vim.api.nvim_create_autocmd("ColorScheme", {
    pattern = "*",
    callback = function()
      vim.defer_fn(tame_ocaml_hl, 1)
    end,
  })

  -- Remove bold from type-related highlights
  local function tame_type_bold()
    vim.api.nvim_set_hl(0, "Type", { fg = "#000000", bold = false })
    vim.api.nvim_set_hl(0, "Include", { fg = "#000000", bold = false })
  end
  vim.api.nvim_create_autocmd("ColorScheme", {
    pattern = "*",
    callback = function()
      vim.defer_fn(tame_type_bold, 1)
    end,
  })

  -- Auto format on save
  local fmt_group = vim.api.nvim_create_augroup("AutoFormat", { clear = true })
  
  vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*.lua",
    command = "Neoformat",
    group = fmt_group,
  })

  vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = {"*.ts", "*.tsx"},
    command = "Neoformat prettier",
    group = fmt_group,
  })

  vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*.js",
    command = "Neoformat eslint_d",
    group = fmt_group,
  })

  vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*.json",
    command = "Neoformat prettier",
    group = fmt_group,
  })

  vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = {"*.res", "*.prisma"},
    callback = function() 
      vim.lsp.buf.format() 
    end,
    group = fmt_group,
  })

  vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*.re",
    command = "Neoformat",
    group = fmt_group,
  })

  vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*.ml",
    command = "Neoformat ocamlformat",
    group = fmt_group,
  })

  -- Telescope history (last 50 files)
  local history_path = vim.fn.expand("~/.telescope_history")
  vim.api.nvim_create_autocmd("BufEnter", {
    pattern = "*",
    callback = function()
      local path = vim.fn.expand("%:p")
      if path == "" or vim.fn.filereadable(path) ~= 1 then
        return
      end
      local ok, lines = pcall(vim.fn.readfile, history_path)
      if not ok then
        lines = {}
      end
      local new_lines = { path }
      for _, line in ipairs(lines or {}) do
        if line ~= path then
          new_lines[#new_lines + 1] = line
        end
        if #new_lines >= 50 then
          break
        end
      end
      pcall(vim.fn.writefile, new_lines, history_path)
    end,
  })
end

-- ============================================================================
-- Module: Keymaps
-- ============================================================================
local Keymaps = {}

function Keymaps.setup()
  local keyOpts = {noremap = true, silent = true}
  local function map(mode, lhs, rhs, opts)
    vim.keymap.set(mode, lhs, rhs, opts or keyOpts)
  end

  -- Better yank/delete
  map("n", "Y", "y$")
  map("n", "D", "^d$")

  -- Move on panes
  -- map("n", "<C-h>", "<C-w>h")
  -- map("n", "<C-l>", "<C-w>l")

  -- Save and close
  map("n", "<C-s>", vim.cmd.write)
  map("i", "<C-s>", function() 
    vim.cmd("write!") 
  end)
  map("n", "<C-w>", vim.cmd.Bdelete)
  map("i", "<C-w>", vim.cmd.Bdelete)
  map("v", "<C-w>", function() vim.cmd("quit!") end)

  -- Move lines in visual mode
  map("v", "<S-J>", ":m '>+2<CR>gv=gv")
  map("v", "<S-K>", ":m '>-1<CR>gv=gv")

  -- Word wrap navigation
  map("n", "k", "v:count == 0 ? 'gk' : 'k'", {noremap = true, expr = true, silent = true})
  map("n", "j", "v:count == 0 ? 'gj' : 'j'", {noremap = true, expr = true, silent = true})

  -- Folding
  map("v", "<leader>w", vim.cmd.fold)

  -- File explorer (Oil)
  map("n", "<leader>n", vim.cmd.Oil)

  -- Buffer navigation (Bufferline)
  map("n", "gt", function() vim.cmd("BufferNext") end)
  map("n", "gT", function() vim.cmd("BufferPrevious") end)
  map("n", "<C-j>", function() vim.cmd("BufferNext") end)
  map("n", "<C-k>", function() vim.cmd("BufferPrevious") end)

  -- Search and replace (Spectre)
  map("n", "<leader>S", function() require('spectre').open() end)

  -- Telescope
  local function find_files_recent_first()
    local pickers = require("telescope.pickers")
    local finders = require("telescope.finders")
    local conf = require("telescope.config").values
    local make_entry = require("telescope.make_entry")
    local sorters = require("telescope.sorters")

    local cwd = vim.fn.getcwd()
    local history_path = vim.fn.expand("~/.telescope_history")
    local repo_name = vim.fn.fnamemodify(cwd, ":t")
    local use_monorepo_filter = repo_name == "monorepo"
    local allowed_prefixes = { "backend/", "frontend/" }
    local ignore_patterns = { "opam", "_build", "node_modules" }

    local function allowed_path(rel)
      if not use_monorepo_filter then
        return true
      end
      for _, pat in ipairs(ignore_patterns) do
        if rel:find(pat, 1, true) then
          return false
        end
      end
      for _, prefix in ipairs(allowed_prefixes) do
        if vim.startswith(rel, prefix) then
          return true
        end
      end
      return false
    end

    local function history_results()
      local results = {}
      local ok, lines = pcall(vim.fn.readfile, history_path)
      if not ok then
        return results
      end
      for _, path in ipairs(lines) do
        local abs = vim.fn.fnamemodify(path, ":p")
        if vim.startswith(abs, cwd .. "/") and vim.fn.filereadable(abs) == 1 then
          local rel = abs:sub(#cwd + 2)
          if allowed_path(rel) then
            results[#results + 1] = rel
          end
        end
      end
      return results
    end

    local history_finder = finders.new_table({
      results = history_results(),
      entry_maker = make_entry.gen_from_file({ cwd = cwd }),
    })

    local find_command = { "rg", "--files", "--hidden" }
    if use_monorepo_filter then
      table.insert(find_command, "-g")
      table.insert(find_command, "backend/**")
      table.insert(find_command, "-g")
      table.insert(find_command, "frontend/**")
      table.insert(find_command, "-g")
      table.insert(find_command, "!*opam*")
      table.insert(find_command, "-g")
      table.insert(find_command, "!*_build*")
      table.insert(find_command, "-g")
      table.insert(find_command, "!*node_modules*")
    end

    local files_finder = finders.new_oneshot_job(find_command, {
      cwd = cwd,
      entry_maker = make_entry.gen_from_file({ cwd = cwd }),
    })

    local base_sorter = conf.file_sorter({})
    local recent_sorter = sorters.new({
      discard = base_sorter.discard,
      start = function(_, prompt)
        base_sorter:_start(prompt)
      end,
      finish = function(_, prompt)
        base_sorter:_finish(prompt)
      end,
      scoring_function = function(_, prompt, line, entry, cb_add, cb_filter)
        if prompt == "" then
          return entry.index or 1
        end
        return base_sorter:scoring_function(prompt, line, entry, cb_add, cb_filter)
      end,
      highlighter = function(_, prompt, display)
        return base_sorter.highlighter(base_sorter, prompt, display)
      end,
    })

    pickers.new({}, {
      prompt_title = "Find Files",
      finder = history_finder,
      sorter = recent_sorter,
      previewer = false,
      on_input_filter_cb = function(prompt)
        if prompt == "" then
          return { updated_finder = history_finder }
        end
        return { updated_finder = files_finder }
      end,
    }):find()
  end

  map("n", "<C-p>", find_files_recent_first)
  map("n", "<leader>t<space>", function() vim.cmd("Telescope buffers") end)
  map("n", "<leader>tp", function() vim.cmd("Telescope find_files") end)
  map("n", "<leader>tl", function() vim.cmd("Telescope live_grep") end)
  map("n", "<leader>tf", function() vim.cmd("Telescope current_buffer_fuzzy_find") end)
  map("n", "<leader>th", function() vim.cmd("Telescope help_tags") end)
  map("n", "<leader>tb", function() vim.cmd("Telescope git_branches") end)
  map("n", "<leader>tg", function() vim.cmd("Telescope git_files") end)
  map("n", "<leader>ts", function() vim.cmd("Telescope git_status") end)
  map("n", "<leader>td", function() vim.cmd("Telescope grep_string") end)
  map("n", "<leader>to", function() vim.cmd("Telescope oldfiles") end)
  map("n", "<leader>tc", function() require('telescope.builtin').tags{ only_current_buffer = true } end)
  map("n", "<leader>ta", function() require('telescope.builtin').lsp_code_actions() end)
  map("n", "<leader>tr", function() vim.cmd("Telescope lsp_references") end)

  -- Git gutter
  local function repo_root_for_path(path)
    local dir = path ~= "" and vim.fn.fnamemodify(path, ":h") or vim.fn.getcwd()
    local root = vim.fn.systemlist({ "git", "-C", dir, "rev-parse", "--show-toplevel" })[1]
    if vim.v.shell_error ~= 0 or not root or root == "" then
      return nil
    end
    return root
  end

  local function get_repo_hunks(root)
    local lines = vim.fn.systemlist({ "git", "-C", root, "diff", "--no-color", "-U0", "--relative" })
    if vim.v.shell_error ~= 0 then
      return {}
    end

    local hunks = {}
    local current_file = nil
    for _, line in ipairs(lines) do
      if vim.startswith(line, "+++ ") then
        local path = line:sub(5)
        if vim.startswith(path, "b/") then
          path = path:sub(3)
        end
        if path == "/dev/null" then
          current_file = nil
        else
          current_file = path
        end
      elseif vim.startswith(line, "@@") and current_file then
        local start, count = line:match("%+([0-9]+),?([0-9]*)")
        local lnum = tonumber(start) or 1
        local num = tonumber(count) or 1
        if num == 0 then
          lnum = lnum > 0 and lnum or 1
        end
        hunks[#hunks + 1] = { file = current_file, line = lnum }
      end
    end

    return hunks
  end

  local function goto_repo_hunk(direction)
    local path = vim.api.nvim_buf_get_name(0)
    local root = repo_root_for_path(path)
    if not root then
      return
    end

    local hunks = get_repo_hunks(root)
    if #hunks == 0 then
      return
    end

    local rel = ""
    if path ~= "" then
      local abs = vim.fn.fnamemodify(path, ":p")
      if vim.startswith(abs, root .. "/") then
        rel = abs:sub(#root + 2)
      end
    end
    local cur_line = vim.api.nvim_win_get_cursor(0)[1]
    local current_index = nil
    for i, hunk in ipairs(hunks) do
      if hunk.file == rel and hunk.line <= cur_line then
        current_index = i
      end
    end

    local target_index
    if direction == "next" then
      if current_index and current_index < #hunks then
        target_index = current_index + 1
      else
        target_index = 1
      end
    else
      if current_index and current_index > 1 then
        target_index = current_index - 1
      else
        target_index = #hunks
      end
    end

    local target = hunks[target_index]
    if not target then
      return
    end

    local fullpath = root .. "/" .. target.file
    vim.cmd("edit " .. vim.fn.fnameescape(fullpath))
    vim.api.nvim_win_set_cursor(0, { target.line, 0 })
  end

  map("n", "<leader>gn", function()
    goto_repo_hunk("next")
  end)
  map("n", "<leader>gp", function()
    goto_repo_hunk("prev")
  end)
  map("n", "<leader>gu", vim.cmd.GitGutterUndoHunk)

  -- Goto preview
  map("n", "<leader>pr", function() require('goto-preview').goto_preview_references() end)
  map("n", "<leader>pd", function() require('goto-preview').goto_preview_definition() end)
  map({"n", "v"}, "<leader>pp", function()
    require('goto-preview').close_all_win()
    vim.cmd("LspRestart")
  end, { buffer=true, desc="Close goto-preview and restart LSP"})

  -- LSP keymaps
  map("n", "K", vim.lsp.buf.hover)
  map("n", "gK", function() 
    vim.cmd("vsplit")
    vim.cmd("Telescope lsp_definitions")
  end)
  map("n", "gd", vim.lsp.buf.declaration)
  map("n", "gD", vim.lsp.buf.definition)
  map("n", "<leader>d", vim.lsp.buf.definition)
  map("n", "gi", vim.lsp.buf.implementation)
  map("n", "gr", function() vim.cmd("Telescope lsp_references") end)
  map("n", "<leader>rn", vim.lsp.buf.rename)
  map("n", "<leader>ca", vim.lsp.buf.code_action)
  map("v", "<leader>ca", vim.lsp.buf.code_action)
  map("n", "<leader>f", vim.lsp.buf.format)
  map("n", "<leader>[", vim.diagnostic.goto_prev)
  map("n", "<leader>]", vim.diagnostic.goto_next)

  -- Theme and config reload
  map("n", "<leader><leader>1", function() 
    vim.cmd("source ~/.config/nvim/init.lua") 
  end)
  map("n", "<leader><leader>d", Theme.set_dark_theme)
  map("n", "<leader><leader>l", Theme.set_light_theme)

  -- OpenCode reference (file:line)
  map("n", "<leader>ol", function()
    local file = vim.fn.expand("%:.")
    if file == "" then
      return
    end
    local line = vim.api.nvim_win_get_cursor(0)[1]
    local ref = string.format("%s:%d", file, line)
    if vim.env.TMUX and vim.env.TMUX ~= "" and vim.fn.executable("tmux") == 1 then
      vim.fn.system({ "tmux", "set-buffer", ref })
      vim.notify("tmux buffer: " .. ref)
    else
      vim.fn.setreg('"', ref)
      vim.notify("register: " .. ref)
    end
  end)

end

-- ============================================================================
-- Module: LSP
-- ============================================================================
local LSP = {}

function LSP.setup()
  -- Diagnostic configuration
  vim.diagnostic.config({
    virtual_text = true,
    signs = true,
    underline = true,
    update_in_insert = false,
    severity_sort = true,
    float = {
      style = "minimal",
      border = "solid",
      source = "always",
      header = "",
      prefix = "",
    },
  })

  -- Define diagnostic signs
  local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
  for type, icon in pairs(signs) do
    local hl = "DiagnosticSign" .. type
    vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
  end

  --Configure LSP handlers
  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(
    vim.lsp.handlers.hover, {
      border = border,
      winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder,ColorColumn:NormalFloat",
    }
  )

  vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(
    vim.lsp.handlers.signature_help, {
      border = border,
      winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder",
    }
  )

  -- Completion setup
  local cmp = require("cmp")

  cmp.setup {
    views = { entries = "native" },
    experimental = { ghost_text = true, native_menu = false },
    window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
    },
    mapping = {
      ["<C-p>"] = cmp.mapping.select_prev_item(),
      ["<C-n>"] = cmp.mapping.select_next_item(),
      ["<C-d>"] = cmp.mapping.scroll_docs(-3),
      ["<C-f>"] = cmp.mapping.scroll_docs(5),
      ["<C-Space>"] = cmp.mapping.complete(),
      ["<C-e>"] = cmp.mapping.close(),
      ["<CR>"] = cmp.mapping.confirm {
        behavior = cmp.ConfirmBehavior.Replace,
        select = true
      },
      ["<S-Tab>"] = function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        else
          fallback()
        end
      end
    },
    sources = {
      { name = "nvim_lsp" },
      { name = "buffer" },
      { name = "path" }
    }
  }

  -- Setup capabilities
  local capabilities = require("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

  -- OCaml LSP
  vim.lsp.config('ocamllsp', {
    capabilities = capabilities,
  })
  vim.lsp.enable('ocamllsp')

  -- Inline completion handled by Minuet (virtual text)

end

-- ============================================================================
-- Module: Plugins
-- ============================================================================
local Plugins = {}

Plugins.specs = {
  -- Git
  "tpope/vim-fugitive",
  "tpope/vim-rhubarb",
  "airblade/vim-gitgutter",
  "lewis6991/gitsigns.nvim",
  "lewis6991/satellite.nvim",
  'kdheepak/lazygit.nvim',
  'sindrets/diffview.nvim',
  {'akinsho/git-conflict.nvim', version = "*", config = true},


  -- Editing
  "tpope/vim-commentary",
  "tpope/vim-surround",
  "tpope/vim-sensible",
  "windwp/nvim-autopairs",
  "andymass/vim-matchup",
  "alvan/vim-closetag",
  "mg979/vim-visual-multi",

  -- Navigation
  "christoomey/vim-tmux-navigator",
  'stevearc/oil.nvim',
  'windwp/nvim-spectre',

  -- UI
  "yuttie/comfortable-motion.vim",
  "famiu/bufdelete.nvim",
  "kyazdani42/nvim-web-devicons",
  "kyazdani42/nvim-tree.lua",
  "lukas-reineke/indent-blankline.nvim",
  "APZelos/blamer.nvim",
  {
    'romgrk/barbar.nvim',
    dependencies = 'nvim-tree/nvim-web-devicons',
  },

  -- Themes

  -- LSP
  "neovim/nvim-lspconfig",
  'rmagatti/goto-preview',
  'folke/trouble.nvim',

  -- Completion
  "hrsh7th/nvim-cmp",
  "hrsh7th/cmp-buffer",
  "hrsh7th/cmp-path",
  "hrsh7th/cmp-cmdline",
  "hrsh7th/cmp-nvim-lsp",

  -- Formatting
  "sbdchd/neoformat",

  -- Language support
  "mattn/emmet-vim",

  -- Telescope
  "nvim-telescope/telescope.nvim",

  -- Dependencies
  "nvim-lua/plenary.nvim",
  "MunifTanjim/nui.nvim",
  "rcarriga/nvim-notify",

  -- OpenCode
  -- { dir = vim.fn.stdpath('config') .. '/lua/opencode' },


  -- AI assistant
  {
	'milanglacier/minuet-ai.nvim',
	config = function()
	  require('minuet').setup({
	    provider = "openai",
	    virtualtext = {
	      auto_trigger_ft = { "*" },
	      keymap = {
	        next = "<M-]>",
	        prev = "<M-[>",
	        dismiss = "<C-]>",
	      },
	    },
	    provider_options = {
	      openai = {
	        model = 'gpt-4.1-mini',
	        stream = true,
	        api_key = function()
	          if not vim.env.OPENAI_API_KEY or vim.env.OPENAI_API_KEY == '' then
	            vim.notify("Minuet: OPENAI_API_KEY is not set", vim.log.levels.WARN)
	            return "MISSING"
	          end
	          return vim.env.OPENAI_API_KEY
	        end,
	        optional = {
	          max_completion_tokens = 128,
	        },
	      }
	    }
	  })

	  vim.keymap.set("i", "<Tab>", function()
	    local vt = require("minuet.virtualtext").action
	    if vt.is_visible() then
	      vt.accept()
	      return ""
	    end
	    return "<Tab>"
	  end, { expr = true, replace_keycodes = true, desc = "Minuet accept" })

	  vim.keymap.set("i", "<S-Tab>", function()
	    local vt = require("minuet.virtualtext").action
	    if vt.is_visible() then
	      vt.accept_line()
	      return ""
	    end
	    return "<S-Tab>"
	  end, { expr = true, replace_keycodes = true, desc = "Minuet accept line" })
	end
  }
}

function Plugins.setup()
  local g = vim.g

  -- Autopairs
  require("nvim-autopairs").setup()
  require("nvim-autopairs").remove_rule("`")

  -- Telescope
  require("telescope").setup {
    pickers = {
      colorscheme = {
        enable_preview = true
      }
    },
    defaults = {
      mappings = {},
      file_ignore_patterns = {"node%_modules/.*", "**/__generated__", "**/*.mjs"}
    },
  }

  -- Goto preview
  require('goto-preview').setup()

  -- Barbar (buffer line)
  require("barbar").setup({
    animation = false,
    auto_hide = false,
    clickable = true,
    icons = {
      filetype = { enabled = true },
      button = "",
      modified = { button = "" },
    },
  })

  -- Oil file explorer
  require("oil").setup({
    columns = { "icon" },
    buf_options = {
      buflisted = false,
      bufhidden = "hide",
    },
    win_options = {
      wrap = false,
      signcolumn = "no",
      cursorcolumn = false,
      foldcolumn = "0",
      spell = false,
      list = false,
      conceallevel = 3,
      concealcursor = "n",
    },
    default_file_explorer = true,
    restore_win_options = true,
    skip_confirm_for_simple_edits = false,
    delete_to_trash = false,
    prompt_save_on_select_new_entry = true,
    keymaps = {
      ["g?"] = "actions.show_help",
      ["<CR>"] = "actions.select",
      ["<C-s>"] = "actions.select_vsplit",
      ["<C-h>"] = "actions.select_split",
      ["<C-t>"] = "actions.select_tab",
      ["<C-p>"] = "actions.preview",
      ["<C-c>"] = "actions.close",
      ["<C-l>"] = "actions.refresh",
      ["-"] = "actions.parent",
      ["_"] = "actions.open_cwd",
      ["`"] = "actions.cd",
      ["~"] = "actions.tcd",
      ["g."] = "actions.toggle_hidden",
    },
    use_default_keymaps = true,
    view_options = {
      show_hidden = false,
      is_hidden_file = function(name, bufnr)
        return vim.startswith(name, ".")
      end,
      is_always_hidden = function(name, bufnr)
        return false
      end,
    },
    float = {
      padding = 4,
      max_width = 0,
      max_height = 0,
      border = "solid",
      win_options = {
        winblend = 10,
      },
    },
    preview = {
      max_width = 0.9,
      min_width = { 40, 0.4 },
      width = nil,
      max_height = 0.9,
      min_height = { 5, 0.1 },
      height = nil,
      border = "rounded",
      win_options = {
        winblend = 0,
      },
    },
    progress = {
      max_width = 0.9,
      min_width = { 40, 0.4 },
      width = nil,
      max_height = { 10, 0.9 },
      min_height = { 5, 0.1 },
      height = nil,
      border = "rounded",
      minimized_border = "none",
      win_options = {
        winblend = 1,
      },
    },
  })

  -- Git blamer
  g.blamer_enabled = 2
  g.blamer_delay = 501
  g.blamer_relative_time = 2
  g.blamer_show_in_insert_modes = 1

  -- Gitsigns (needed for satellite git markers)
  require("gitsigns").setup({
    signcolumn = false,
    numhl = false,
    linehl = false,
    word_diff = false,
  })

  -- Satellite scrollbar
  require("satellite").setup({
    width = 3,
    handlers = {
      gitsigns = { enabled = true },
    },
  })

  -- Close tag
  g.closetag_filenames = "*.html,*.tsx,*.re"

  -- Emmet
  g.user_emmet_leader_key = "<C-e>"
  g.user_emmet_settings = {
    ["reason"] = {
      ["extends"] = 'jsx',
    }
  }

  -- Neoformat
  g.neoformat_lua_customLuafmt = {
    exe = "luafmt",
    args = {"--indent-count", 3, "--stdin"},
    stdin = 2
  }
  g.neoformat_enabled_lua = {"customLuafmt"}
  g.neoformat_only_msg_on_error = 2

end

-- ============================================================================
-- Bootstrap & Main
-- ============================================================================

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Suppress deprecation warnings from third-party plugins
vim.deprecate = function() end

-- Initialize configuration
Options.setup()
require("lazy").setup(Plugins.specs)
Autocmds.setup()
Keymaps.setup()
LSP.setup()
Plugins.setup()
Theme.setup()

