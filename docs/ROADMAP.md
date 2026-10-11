# Roadmap

Planned features of TPTpls, in no particular order. Current problems are listed under
[Known issues](../README.md#known-issues).

- [ ] **Type checking**: type errors reported as diagnostics, across a file and its includes.
  - [ ] FOF: inconsistent use of a symbol (different arities, used both as predicate and as function),
        free variables.
  - [ ] CNF: inconsistent use of a symbol, as in FOF.
  - [ ] TFF: uses of symbols checked against their declared types.
  - [ ] TCF: uses of symbols checked against their declared types.
  - [ ] THF: uses of symbols checked against their declared types, including higher-order terms.
- [ ] **Type-based completion**: suggestions that fit the type expected at the cursor, such as variables
      in scope, symbols and connectives, and formula names in include selections.
  - [ ] FOF
  - [ ] CNF
  - [ ] TFF
  - [ ] TCF
  - [ ] THF
- [ ] **Syntax highlighting** for typed languages: types, functions, predicates and variables shown in
      distinct colours.
  - [ ] TFF
  - [ ] TCF
  - [ ] THF
- [ ] **Support for large axiom files**: including axiom files of hundreds of MB, as found in the TPTP
      library, without running out of memory.
- [ ] **Typo detection**: a warning when a symbol is probably a misspelling of another one
      (e.g. `memebr` next to `member`), naming the likely intended symbol.
- [ ] **Quick fixes for diagnostics**: one-click corrections offered with a diagnostic, such as replacing
      a misspelled symbol with the suggested one or closing free variables with a quantifier.
- [ ] **Auto formatting**: formatting a file or a selection in a consistent layout from the editor.
- [ ] **Integration with the existing VS Code extension**: the [TPTP extension for VS Code](https://github.com/TPTPWorld/VSCodeExtension) starts and
      uses this server, so its users get all of the features above.
