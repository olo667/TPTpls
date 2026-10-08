-- Minimal Neovim config for trying and showing the TPTP language server from this repository.
--
--   sbt server/assembly                                     # build the jar first
--   nvim -u editors/nvim/init.lua editors/demo/showcase.p   # does not load your own config
--
-- TPTP root for includes: $TPTP, else ~/.cache/tptp-lsp/TPTP-v9.3.1 (see scripts/fetch-tptp.sh).
-- Keys: <leader>? (space ?) lists them all · :TptpLog opens the LSP log.

local this = debug.getinfo(1, 'S').source:sub(2)
local repo = vim.fn.fnamemodify(this, ':p:h:h:h')
local jar = repo .. '/server/target/scala-3.3.8/tptp-lsp.jar'

local tptp_root = os.getenv('TPTP')
local cached = vim.fn.expand('~/.cache/tptp-lsp/TPTP-v9.3.1')
if (tptp_root == nil or tptp_root == '') and vim.fn.isdirectory(cached) == 1 then tptp_root = cached end

vim.g.mapleader = ' '
vim.opt.number = true
vim.opt.signcolumn = 'yes'
vim.opt.cursorline = true
vim.opt.mouse = 'a'
vim.opt.updatetime = 500 -- CursorHold, which shows the diagnostic under the cursor
vim.opt.laststatus = 2
if vim.lsp.log and vim.lsp.log.set_level then vim.lsp.log.set_level('info') else vim.lsp.set_log_level('info') end

-- the float lists a diagnostic's related locations, e.g. where the errors of an included file are
local function with_related(d)
  local related = vim.tbl_get(d, 'user_data', 'lsp', 'relatedInformation') or {}
  local lines = { d.message }
  for _, r in ipairs(related) do
    local file = vim.fn.fnamemodify(vim.uri_to_fname(r.location.uri), ':~:.')
    table.insert(lines, string.format('  → %s:%d: %s', file, r.location.range.start.line + 1, r.message))
  end
  return table.concat(lines, '\n')
end

vim.diagnostic.config({
  virtual_text = { prefix = '●' },
  underline = true,
  severity_sort = true,
  update_in_insert = false, -- shown on leaving insert mode (use Esc: Ctrl-C skips InsertLeave)
  signs = { text = { [vim.diagnostic.severity.ERROR] = 'E', [vim.diagnostic.severity.WARN] = 'W' } },
  float = { border = 'rounded', source = false, header = '', format = with_related },
})

-- .p (normally Pascal) and .ax files are TPTP
vim.filetype.add({ extension = { p = 'tptp', ax = 'tptp' } })

-- just enough highlighting to read TPTP comfortably
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'tptp',
  callback = function()
    vim.bo.commentstring = '%%s'
    vim.cmd([[
      syntax match tptpComment /%.*$/
      syntax region tptpComment start=+/\*+ end=+\*/+
      syntax match tptpKeyword /\<\(thf\|tff\|tcf\|fof\|cnf\|tpi\|include\)(/me=e-1
      syntax match tptpVariable /\<[A-Z][A-Za-z0-9_]*\>/
      syntax match tptpDefined /\$\$\?[a-z][A-Za-z0-9_]*/
      syntax region tptpQuoted start=+'+ skip=+\\'+ end=+'+
      highlight default link tptpComment Comment
      highlight default link tptpKeyword Keyword
      highlight default link tptpVariable Identifier
      highlight default link tptpDefined Constant
      highlight default link tptpQuoted String
    ]])
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  pattern = 'tptp',
  callback = function(args)
    if vim.fn.filereadable(jar) == 0 then
      vim.notify('tptp-lsp: ' .. jar .. ' not found; run `sbt server/assembly` in ' .. repo, vim.log.levels.ERROR)
      return
    end
    local file = vim.api.nvim_buf_get_name(args.buf)
    vim.lsp.start({
      name = 'tptp-lsp',
      cmd = { 'java', '-jar', jar },
      root_dir = vim.fs.root(args.buf, { '.git' }) or vim.fs.dirname(file),
      init_options = { tptpRoot = tptp_root },
    })
  end,
})

-- outline: the document symbols in a location list on the right; <CR> jumps to a record
local function outline()
  vim.lsp.buf.document_symbol({
    on_list = function(result)
      vim.fn.setloclist(0, {}, ' ', { title = 'Outline', items = result.items })
      vim.cmd('vertical lopen 45')
    end,
  })
end

local help = {
  'TPTP language server: keys',
  '',
  '  gd         go to definition (include file name, selected formula name)',
  '  <C-o>      jump back',
  '  <space>o   outline of the file (<CR> jumps, q closes)',
  '  ]d  [d     next / previous diagnostic',
  '  <space>e   diagnostic under the cursor (also shown when the cursor rests)',
  '  <space>q   all diagnostics of the file in a list',
  '  <space>?   this help',
  '',
  '  :TptpLog   open the LSP log',
  '',
  'Diagnostics update while you type (outside insert mode);',
  'leave insert mode with Esc to see them.',
}

local function show_help()
  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, help)
  vim.bo[buf].modifiable = false
  local width = 0
  for _, l in ipairs(help) do width = math.max(width, vim.fn.strdisplaywidth(l)) end
  local win = vim.api.nvim_open_win(buf, true, {
    relative = 'editor', style = 'minimal', border = 'rounded',
    width = width + 2, height = #help,
    row = math.floor((vim.o.lines - #help) / 2), col = math.floor((vim.o.columns - width) / 2),
  })
  for _, key in ipairs({ 'q', '<Esc>', '<CR>' }) do
    vim.keymap.set('n', key, function() vim.api.nvim_win_close(win, true) end, { buffer = buf })
  end
end

vim.api.nvim_create_user_command('TptpHelp', show_help, {})
vim.api.nvim_create_user_command('TptpLog', function() vim.cmd.edit(vim.lsp.get_log_path()) end, {})
vim.keymap.set('n', '<leader>?', show_help)

local announced = false
vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local opts = { buffer = args.buf }
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', '<leader>o', outline, opts)
    vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, opts)
    vim.keymap.set('n', ']d', function() vim.diagnostic.jump({ count = 1, float = true }) end, opts)
    vim.keymap.set('n', '[d', function() vim.diagnostic.jump({ count = -1, float = true }) end, opts)
    vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)
    vim.api.nvim_create_autocmd('CursorHold', {
      buffer = args.buf,
      callback = function() vim.diagnostic.open_float({ scope = 'cursor', focus = false }) end,
    })
    if not announced then
      announced = true
      vim.notify('tptp-lsp attached · press <space>? for the keys')
    end
  end,
})

-- status line: file, language server state, error and warning counts, position
function _G.tptp_status()
  local attached = #vim.lsp.get_clients({ bufnr = 0, name = 'tptp-lsp' }) > 0
  if not attached then return '' end
  local count = vim.diagnostic.count(0)
  local errors = count[vim.diagnostic.severity.ERROR] or 0
  local warnings = count[vim.diagnostic.severity.WARN] or 0
  return string.format('tptp-lsp · E %d  W %d', errors, warnings)
end
vim.opt.statusline = ' %f %m%r%= %{v:lua.tptp_status()}   %l:%c '
vim.api.nvim_create_autocmd('DiagnosticChanged', { callback = function() vim.cmd.redrawstatus() end })
