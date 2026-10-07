include('Axioms/SET001+0.ax').
include('Axioms/SET001+1.ax', [ax1, ax2]).
thf(t, type, c: $i).
tff(f, axiom, ! [X: $int] : $less(X, $sum(X, X))).
cnf(c1, axiom, p(X) | ~ q(X)).
