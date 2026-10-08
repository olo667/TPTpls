%------------------------------------------------------------------------------
% TPTP language server showcase. Open with:
%   nvim -u editors/nvim/init.lua editors/demo/showcase.p
% Keys: <space>? help · <space>o outline · gd definition · ]d / [d diagnostics
%------------------------------------------------------------------------------

% Go to definition: put the cursor on a file name or a selected name and press gd
% (<C-o> jumps back). 'no_such_axiom' does not exist and is reported.
include('axioms/sets.ax', [member_def, subset_def, no_such_axiom]).

% A file that cannot be found (searched: this directory, workspace, TPTP root).
include('axioms/missing.ax').

% An included file with a syntax error: the error is reported here, with a
% link to where it is (shown in the diagnostic float).
include('axioms/broken.ax').

% An include cycle: loop_a.ax includes loop_b.ax, which includes loop_a.ax.
include('axioms/loop_a.ax').

% Error repair: each broken formula keeps its structure and gets one precise error.
fof(missing_operand, axiom, ! [X] : (p(X) & )).
fof(missing_paren, axiom, ! [X] : (p(X) => q(X)).
fof(empty_variables, axiom, ! [] : p(a)).
fof(contained, axiom, r(a) => (q(b) $ $ $ s(c))).

% Records of other languages appear in the outline too.
tff(nat_type, type, nat: $tType).
cnf(clause, axiom, p(X) | ~ q(X)).

fof(goal, conjecture, ! [S, T] : (subset(S, T) => ! [E] : (member(E, S) => member(E, T)))).
