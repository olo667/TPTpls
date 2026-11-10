%------------------------------------------------------------------------------
% File     : ARI683_1 : TPTP v9.3.1. Released v6.3.0.
% Domain   : Arithmetic
% Problem  : Solve system of nonlinear inequalities
% Version  : Especial.
% English  :

% Refs     : [BHS07] Beckert et al. (2007), Verification of Object-Oriented
%          : [Rue14] Ruemmer (2014), Email to Geoff Sutcliffe
% Source   : [Rue14]
% Names    : quadraticInEq9.pri [BHS07]
%          : quadraticInEq9.p [Rue14]

% Status   : Theorem
% Rating   : 0.75 v9.3.0, 0.50 v9.1.0, 0.60 v9.0.0, 0.50 v8.2.0, 0.67 v8.1.0, 0.50 v7.5.0, 0.60 v7.4.0, 0.33 v7.3.0, 0.50 v7.0.0, 0.67 v6.4.0, 1.00 v6.3.0
% Syntax   : Number of formulae    :   16 (   7 unt;   9 typ;   0 def)
%            Number of atoms       :    7 (   0 equ)
%            Maximal formula atoms :    1 (   1 avg)
%            Number of connectives :    0 (   0   ~;   0   |;   0   &)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%            Maximal formula depth :    1 (   1 avg)
%            Maximal term depth    :    6 (   2 avg)
%            Number arithmetic     :   35 (   7 atm;  23 fun;   5 num;   0 var)
%            Number of types       :    1 (   0 usr;   1 ari;   0 dat;   0 cdt)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    2 (   0 usr;   0 prp; 2-2 aty)
%            Number of functors    :   12 (   9 usr;  10 con; 0-2 aty)
%            Number of variables   :    0 (   0   !;   0   ?;   0   :)
% SPC      : TF0_THM_NEQ_ARI_NDT

% Comments : KeY arithmetic regression test, http://www.key-project.org
%------------------------------------------------------------------------------
tff(b_type,type,
    b: $int ).

tff(c_type,type,
    c: $int ).

tff(g_type,type,
    g: $int ).

tff(h_type,type,
    h: $int ).

tff(a_type,type,
    a: $int ).

tff(f_type,type,
    f: $int ).

tff(i_type,type,
    i: $int ).

tff(d_type,type,
    d: $int ).

tff(e_type,type,
    e: $int ).

tff(conj,axiom,
    $lesseq($sum($product(a,f),$product(-1,$product(a,h))),$sum($sum(b,$product(c,g)),$product(-1,$product(c,h)))) ).

tff(conj_001,axiom,
    $lesseq(g,f) ).

tff(conj_002,axiom,
    $less(h,g) ).

tff(conj_003,axiom,
    $less(i,h) ).

tff(conj_004,axiom,
    $lesseq(a,d) ).

tff(conj_005,axiom,
    $less(e,a) ).

tff(conj_006,conjecture,
    $lesseq($sum($product(e,f),$product(-1,$product(e,i))),$sum($sum($sum($sum(b,$product(c,g)),$product(-1,$product(c,h))),$product(d,h)),$product(-1,$product(d,i)))) ).

%------------------------------------------------------------------------------
