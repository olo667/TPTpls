-- Minimal Neovim config for trying the TPTP language server from this repository.
--
--   sbt server/assembly                                  # build the jar first
--   nvim -u editors/nvim/init.lua path/to/problem.p      # does not load your own config
--
-- TPTP root for includes: $TPTP, else ~/.cache/tptp-lsp/TPTP-v9.3.1 (see scripts/fetch-tptp.sh).
-- Keys: gd definition · <leader>o outline · <leader>e diagnostic details · ]d / [d next / previous
--       diagnostic · <leader>q all diagnostics in the location list · :TptpLog opens the LSP log.

local this = debug.getinfo(1, 'S').source:sub(2)
local repo = vim.fn.fnamemodify(this, ':p:h:h:h')
local jar = repo .. '/server/target/scala-3.3.8/tptp-lsp.jar'

local tptp_root = os.getenv('TPTP')
local cached = vim.fn.expand('~/.cache/tptp-lsp/TPTP-v9.3.1')
if (tptp_root == nil or tptp_root == '') and vim.fn.isdirectory(cached) == 1 then tptp_root = cached end

vim.g.mapleader = ' '
vim.opt.number = true
vim.opt.signcolumn = 'yes'
if vim.lsp.log and vim.lsp.log.set_level then vim.lsp.log.set_level('info') else vim.lsp.set_log_level('info') end
vim.diagnostic.config({ virtual_text = true, underline = true, severity_sort = true })

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

vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(args)
    local opts = { buffer = args.buf }
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', '<leader>o', vim.lsp.buf.document_symbol, opts)
    vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, opts)
    vim.keymap.set('n', ']d', function() vim.diagnostic.jump({ count = 1, float = true }) end, opts)
    vim.keymap.set('n', '[d', function() vim.diagnostic.jump({ count = -1, float = true }) end, opts)
    vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, opts)
  end,
})

vim.api.nvim_create_user_command('TptpLog', function() vim.cmd.edit(vim.lsp.get_log_path()) end, {})
