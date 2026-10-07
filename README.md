# TPTpls — a language server for TPTP

An LSP server for the [TPTP](https://tptp.org) language, written in Scala 3.
Status: v0.01 — syntax diagnostics, document outline, include resolution and include navigation. No type checking yet.

## Build and run

Requires JDK 21 and sbt.

```sh
sbt test             # all tests
sbt server/assembly  # builds server/target/scala-3.3.8/tptp-lsp.jar
java -jar server/target/scala-3.3.8/tptp-lsp.jar   # speaks LSP on stdin/stdout
```

## Settings

Passed as `initializationOptions` or via `workspace/configuration` (section `tptp`):

| Setting | Default | Meaning |
|---|---|---|
| `tptpRoot` | env `TPTP` | TPTP library root, searched last for `include`s |
| `debounceMs` | `200` | delay after the last edit before re-checking |

Includes are resolved against the including file's directory, then each workspace folder, then `tptpRoot`.

## Neovim

```lua
vim.filetype.add({ extension = { p = 'tptp', ax = 'tptp' } })
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'tptp',
  callback = function(args)
    vim.lsp.start({
      name = 'tptp-lsp',
      cmd = { 'java', '-jar', '/path/to/tptp-lsp.jar' },
      root_dir = vim.fs.root(args.buf, { '.git' }) or vim.fn.getcwd(),
      init_options = { tptpRoot = os.getenv('TPTP') },
    })
  end,
})
```

## Known issues

This is an alpha. Known problems, to be addressed in later versions:

- **Large files use a lot of memory.** The parse tree takes about 180 bytes per byte of text, and
  included files are cached with their full tree. Including an axiom file of several MB (some TPTP
  axiom sets are hundreds of MB) can exhaust the heap.
- **Very deep nesting fails.** Formulas nested about 800 levels deep overflow the analysis thread's
  stack; the file then gets no diagnostics.
- **The TPTP library test has no size limit**, so running it on the whole library fails on the largest
  files. The mutation runner skips files over 256 KB instead (`--max-size-kb`).
- **Some repairs are imprecise.** A deleted `)` is often reported at a later position where it would
  also fit; a wrong operator (`p § q`, `~ [X]` instead of `! [X]`) is reported with ANTLR's generic
  messages, or as two unexpected tokens.
- **A closed document can keep its diagnostics** if its analysis finishes at the moment it is closed.
- **Settings pulled via `workspace/configuration` are not re-read** when the client reports a change;
  restart the server after changing `tptpRoot`, which also cannot be unset once set.
- The "cannot find included file" message can list the same directory twice.
- Only `file:` documents are analysed; unsaved, untitled buffers get no diagnostics.
- Comments are lexed but not yet used (no hover or folding).
- **Very large records are slow to re-check.** A single record of several hundred KB (e.g. the LCL
  modal-logic encodings, up to 250 KB per formula) takes seconds to parse, and longer when broken.
- **One TPTP library file does not parse with the official grammar:** `Problems/SYN/SYN000+2.p` uses
  `theory(equality)` as an inference parent, but the grammar's `external_source` only allows `file(...)`.
  Reported upstream.
- **Testing covered FOF only.** The parser handles every TPTP language (THF, TFF, TCF, FOF, CNF, TPI)
  with the same error repair, outline and include handling, and should work on all of them, but the
  mutation tests and the library test were run on FOF files (TPTP names containing `+`) only; the
  other languages are covered by a few hand-written test inputs.

## Grammar

`core/src/main/antlr4/TPTP.g4` is the official TPTP grammar. Local edits are made only with
maintainer-side confirmation and are reported upstream.

Local modifications:

- Comments (`Comment_line`, `Comment_block`) go to lexer channel 2 instead of being skipped, so the
  server can see them. ANTLR does not allow named channels in a combined grammar, hence the number.

- `Not_star_slash` is declared a `fragment`. Upstream it is a standalone lexer token, so a `**`
  later in a file (e.g. a `%*****` banner) swallows the text before it.
- `Exp_integer` and `Signed_exp_integer` are declared `fragment`s. Upstream they are standalone
  tokens defined before `Integer`, so integers never lex as `Integer`.

Files of the TPTP library that still fail to parse are listed in
`core/src/test/resources/corpus-known-failures.txt`.

## Tests

- `UPDATE_GOLDEN=1 sbt 'core/testOnly tptp.syntax.GoldenSuite'` regenerates CST snapshots (review the diff!).
- `TPTP=/path/to/TPTP sbt 'core/testOnly tptp.syntax.CorpusSuite'` parses the whole TPTP library's FOF files.
- `sbt 'core/testOnly tptp.mutation.*'` runs the mutation tests on the committed TPTP sample
  (`core/src/test/resources/corpus`, included in cooperation with TPTP; re-create it with
  `scripts/fetch-sample.sh`).

## Mutation testing

Each corpus file is mutated many times (seeded, reproducible) and every mutant is parsed:

| Mutation | Must produce an error | Precise result |
|---|---|---|
| delete a term, formula or variable | yes | `MissingElement` at the gap |
| delete `)`, `]` or `).` | yes | `MissingToken` of that kind at the gap ("displaced" if elsewhere) |
| insert a stray `§` | yes | `UnexpectedToken` holding it |
| duplicate a `,` | yes | an error at the extra comma |
| truncate inside a record | yes | an error at the end |
| typo in a keyword, name or operator | no (may still be valid) | an error at the typo |

Invariants fail the run: no exception, at most 2 s per file, no error before the mutated record
(the record before it for keyword typos), and all other records parse exactly as before.
Precision is reported, not enforced.

Full run on the TPTP library (downloads ~1 GB once into `~/.cache/tptp-lsp`):

```sh
sbt "core/Test/runMain tptp.mutation.MutationRun --tptp $(scripts/fetch-tptp.sh) --per-kind 5 --seed 1"
```

Results for v0.0.1 (TPTP v9.3.1, all 10,156 FOF problem and axiom files up to 256 KB, 5 mutants per
kind and file, seed 1, 300,618 mutants in 27 minutes): no crashes, no errors before the mutated record
and no changes to other records; 167 mutants in 29 files with records of up to 250 KB exceeded the time
limit.

| Mutation | Exact | Displaced | Other error | No error |
|---|---:|---:|---:|---:|
| delete a term, formula or variable | 99.4% | – | 0.6% | – |
| delete `)`, `]` or `).` | 67.8% | 31.6% | 0.5% | – |
| insert a stray token | 99.2% | – | 0.8% | – |
| duplicate a comma | 100% | – | – | – |
| truncate inside a record | 100% | – | – | – |
| typo | 29.7% | – | 1.1% | 69.2% (still valid) |

Options: `--per-kind N`, `--seed S`, `--files N` (first N files only), `--max-size-kb N` (default 256),
`--parallel-kb N` (total size of files mutated at once, default 6144; bounds memory), `--out DIR`
(default `target/mutation-report`). The report (`report.md`, `mutants.csv`, `unparsable.txt`) lists
every invariant violation and examples of imprecise results; the exit code is 1 if any invariant fails.
