# TPTP Language Support

Diagnostics, document outline and include navigation for [TPTP](https://tptp.org) files (`.p`, `.ax`, `.tptp`),
from the [TPTpls](https://github.com/olo667/TPTpls) language server.

Requires Java 21 or later on the `PATH`.

Includes are resolved against the including file's directory, then each workspace folder, then the TPTP root
(`tptp.tptpRoot`, else the `TPTP` environment variable).
