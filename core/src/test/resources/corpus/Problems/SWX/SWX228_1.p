%------------------------------------------------------------------------------
% File     : SWX228_1 : TPTP v9.3.1. Released v9.3.0.
% Domain   : Software Verification
% Problem  : Faulty property of the function union
% Version  : Especial.
% English  :

% Refs     : [CST26] Claessen et al. (2026), Email to Geoff Sutcliffe
% Source   : [CST26]
% Names    : Definitions_prop_union_comm.p [CST26]

% Status   : Theorem
% Rating   : 0.67 v9.3.0
% Syntax   : Number of formulae    :   18 (  13 unt;   1 typ;   0 def)
%            Number of atoms       :   54 (   9 equ)
%            Maximal formula atoms :    2 (   3 avg)
%            Number of connectives :   13 (   7   ~;   1   |;   0   &)
%                                         (   3 <=>;   2  =>;   0  <=;   0 <~>)
%            Maximal formula depth :    6 (   3 avg)
%            Maximal term depth    :    1 (   1 avg)
%            Number of FOOLs       :   37 (  33 fml;   4 var)
%            Number of types       :    1 (   0 usr)
%            Number of type conns  :    2 (   1   >;   1   *;   0   +;   0  <<)
%            Number of predicates  :   13 (  12 usr;   3 prp; 0-2 aty)
%            Number of functors    :    0 (   0 usr;   0 con; --- aty)
%            Number of variables   :   27 (  25   !;   2   ?;   2   :)
% SPC      : TX0_THM_EQU_NAR_NDT

% Comments :
%------------------------------------------------------------------------------
tff(type_001,type,
    barbar: ( $o * $o ) > $o ).

tff(axiom_002,axiom,
    ! [X,X2] : ( head(cons(X,X2)) = X ) ).

tff(axiom_003,axiom,
    ! [X,X2] : ( tail(cons(X,X2)) = X2 ) ).

tff(axiom_004,axiom,
    ! [X,X2] : ( nil != cons(X,X2) ) ).

tff(axiom_005,axiom,
    ! [X] : ( proj1S(s(X)) = X ) ).

tff(axiom_006,axiom,
    ! [X] : ( s(X) != z ) ).

tff(axiom_007,axiom,
    ! [X: $o,Y: $o] :
      ( barbar((X),(Y))
    <=> ( (X)
        | (Y) ) ) ).

tff(axiom_008,axiom,
    otherwise ).

tff(axiom_009,axiom,
    ! [Z,Y2] :
      ( eqNat(s(Z),s(Y2))
    <=> eqNat(Z,Y2) ) ).

tff(axiom_010,axiom,
    ! [Z] : ~ eqNat(s(Z),z) ).

tff(axiom_011,axiom,
    ! [X2] : ~ eqNat(z,s(X2)) ).

tff(axiom_012,axiom,
    eqNat(z,z) ).

tff(axiom_013,axiom,
    ! [X] : ~ elem(X,nil) ).

tff(axiom_014,axiom,
    ! [X,Z,Xs] :
      ( elem(X,cons(Z,Xs))
    <=> barbar(eqNat(X,Z),elem(X,Xs)) ) ).

tff(axiom_015,axiom,
    ! [Y] : ( union(nil,Y) = Y ) ).

tff(axiom_016,axiom,
    ! [Y,Z,Xs] :
      ( elem(Z,Y)
     => ( union(cons(Z,Xs),Y) = union(Xs,Y) ) ) ).

tff(axiom_017,axiom,
    ! [Y,Z,Xs] :
      ( ~ elem(Z,Y)
     => ( union(cons(Z,Xs),Y) = cons(Z,union(Xs,Y)) ) ) ).

tff(goal_018,conjecture,
    ? [Xs,Ys] : ( union(Xs,Ys) != union(Ys,Xs) ) ).

%------------------------------------------------------------------------------
