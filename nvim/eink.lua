-- E-ink theme configuration
local M = {}

function M.set_eink_theme()
  vim.cmd [[
    highlight clear
    set background=light

    " General editor settings
    highlight Normal guifg=#000000 guibg=#FFFFFF
    highlight NormalInactive guifg=#000000 guibg=#FFFFFF
    highlight NormalFloat guifg=#000000 guibg=#FFFFFF
    highlight LineNr guifg=#444444 guibg=#FFFFFF
    highlight CursorLineNr guifg=#111111 guibg=#EEEEEE
    highlight CursorLine guibg=#EEEEEE
    highlight Visual guibg=#CCCCCC
    highlight StatusLine guifg=#000000 guibg=#DDDDDD
    highlight VertSplit guifg=#888888 guibg=#FFFFFF
    highlight SignColumn guibg=#FFFFFF

    " Syntax groups
    highlight Comment guifg=#555555 gui=italic
    highlight Constant guifg=#000000 gui=bold
    highlight Identifier guifg=#000000
    highlight Function guifg=#000000 gui=bold
    highlight Statement guifg=#222222 gui=bold
    highlight Keyword guifg=#111111 gui=bold

    " Clear highlighting for primitives
    highlight Type guifg=NONE guibg=NONE gui=NONE
    highlight String guifg=NONE guibg=NONE gui=NONE

    " LSP diagnostics
    highlight DiagnosticError guifg=#FF0000 guibg=#FFFFFF gui=bold
    highlight DiagnosticWarn guifg=#FF9900 guibg=#FFFFFF gui=bold
    highlight DiagnosticInfo guifg=#0000FF guibg=#FFFFFF gui=bold
    highlight DiagnosticHint guifg=#00AA00 guibg=#FFFFFF gui=bold

    " LSP diagnostics signs
    highlight DiagnosticSignError guifg=#FF0000 guibg=NONE
    highlight DiagnosticSignWarn guifg=#FF9900 guibg=NONE
    highlight DiagnosticSignInfo guifg=#0000FF guibg=NONE
    highlight DiagnosticSignHint guifg=#00AA00 guibg=NONE

    " Completion menu
    highlight Pmenu guifg=#000000 guibg=#FFFFFF
    highlight PmenuSel guifg=#FFFFFF guibg=#000000
    highlight PmenuSbar guibg=#CCCCCC
    highlight PmenuThumb guibg=#888888
  ]]

  vim.api.nvim_set_hl(0, "@markup.raw.block.markdown", { fg = "#ffffff", bg = "#000000", bold = true })
  vim.api.nvim_set_hl(0, "RenderMarkdownCode", { bg = "#000000", fg = "#ffffff" })
  vim.api.nvim_set_hl(0, "NormalFloat", { bg = "#000000", fg = "#ffffff", bold = true })
  vim.api.nvim_set_hl(0, "FloatBorder", { bg = "#000000", fg = "#ffffff" })

  vim.defer_fn(function()
    vim.cmd([[
      highlight! NormalFloat guifg=#FFFFFF guibg=#666666
      highlight! FloatBorder guifg=#FFFFFF guibg=#666666
      highlight! FloatTitle guifg=#FFFFFF guibg=#666666 gui=bold
    ]])
  end, 100)
end

function M.apply_eink_theme()
  M.set_eink_theme()

  local force_theme = function()
    vim.cmd([[
      highlight! NormalFloat guifg=#FFFFFF guibg=#666666
      highlight! FloatBorder guifg=#FFFFFF guibg=#666666
      highlight! FloatTitle guifg=#FFFFFF guibg=#666666 gui=bold

      " Avante highlights
      highlight! AvanteTitle guifg=#000000 guibg=#FFFFFF gui=bold
      highlight! AvanteNormal guifg=#000000 guibg=#FFFFFF
      highlight! AvanteBorder guifg=#333333 guibg=#FFFFFF
      highlight! AvanteContent guifg=#000000 guibg=#FFFFFF
      highlight! AvanteInput guifg=#000000 guibg=#FFFFFF
      highlight! AvanteChat guifg=#000000 guibg=#FFFFFF
      highlight! AvanteSidebar guifg=#000000 guibg=#FFFFFF
    ]])
  end

  force_theme()
  vim.defer_fn(force_theme, 100)
  vim.defer_fn(force_theme, 500)
  vim.defer_fn(force_theme, 1000)
end

function M.fix_float_highlights()
  vim.cmd([[highlight! NormalFloat guifg=#FFFFFF guibg=#666666]])
  vim.cmd([[highlight! FloatBorder guifg=#FFFFFF guibg=#666666]])
end

function M.force_white_theme()
  local white = "#FFFFFF"
  local black = "#000000"

  local groups = {
    "Normal", "NormalNC", "NormalFloat", "FloatBorder", "FloatTitle",
    "Pmenu", "PmenuSel", "PmenuSbar", "PmenuThumb",
    "LazyNormal", "LazyBorder", "LazyTitle", "LazyH1", "LazyButton", "LazyButtonActive",
    "MasonNormal", "MasonBorder", "MasonHeader",
    "LspInfoBorder", "LspSignatureHelpBorder",
  }

  for _, group in ipairs(groups) do
    if group == "PmenuSel" or group == "LazyButtonActive" then
      vim.api.nvim_set_hl(0, group, { fg = white, bg = black, force = true })
    else
      vim.api.nvim_set_hl(0, group, { fg = black, bg = white, force = true })
    end
  end
end

function M.set_dark_theme()
  vim.cmd [[
    color themer_ayu_dark
    set background=light
    hi Visual guifg=DarkGrey guibg=DarkBlue gui=none
  ]]
end

function M.debug_highlights()
  local groups = {"NormalFloat", "FloatBorder", "FloatTitle", "AvanteTitle", "AvanteNormal"}
  for _, group in ipairs(groups) do
    local hl = vim.api.nvim_get_hl(0, {name = group})
    print(string.format("%s: fg=%s bg=%s", group, hl.fg or "none", hl.bg or "none"))
  end
end

function M.setup()
  -- Apply theme immediately
  M.apply_eink_theme()
  M.force_white_theme()

  -- Set up autocmds for theme enforcement
  local theme_group = vim.api.nvim_create_augroup("EinkThemeEnforcement", { clear = true })
  
  vim.api.nvim_create_autocmd("ColorScheme", {
    group = theme_group,
    callback = function()
      M.apply_eink_theme()
      M.force_white_theme()
    end,
  })

  vim.api.nvim_create_autocmd("VimEnter", {
    group = theme_group,
    callback = M.apply_eink_theme,
  })

  vim.api.nvim_create_autocmd("User", {
    pattern = "LazyLoad",
    group = theme_group,
    callback = function()
      vim.defer_fn(M.apply_eink_theme, 100)
    end,
  })

  vim.api.nvim_create_autocmd("User", {
    pattern = "LazyDone",
    group = theme_group,
    callback = function()
      vim.defer_fn(M.apply_eink_theme, 200)
    end,
  })

  vim.api.nvim_create_autocmd("BufWinEnter", {
    group = theme_group,
    callback = function()
      vim.defer_fn(M.fix_float_highlights, 50)
    end,
  })

  -- Markdown code block highlighting
  vim.api.nvim_set_hl(0, "@markup.raw.block.markdown", { fg = "#ffffff", bg = "#000000", bold = true })
  vim.api.nvim_set_hl(0, "RenderMarkdownCode", { bg = "#000000", fg = "#ffffff" })
end

return M
