
%------------------------------------------------------------------------------
% File     : GRP001+6 : TPTP v9.3.1. Released v3.1.0.
% Domain   : Group Theory
% Problem  : X^2 = identity => commutativity
% Version  : Especial.
% English  : If the square of every element is the identity, the system
%            is commutative.

% Refs     : [Shu04] Shults (2004), Email to G. Sutcliffe
% Source   : [Shu04]
% Names    :

% Status   : Theorem
% Rating   : 0.00 v9.3.0, 0.10 v9.1.0, 0.07 v9.0.0, 0.00 v7.5.0, 0.14 v7.4.0, 0.06 v7.3.0, 0.00 v7.0.0, 0.14 v6.4.0, 0.07 v6.3.0, 0.08 v6.2.0, 0.18 v6.1.0, 0.24 v6.0.0, 0.50 v5.5.0, 0.21 v5.4.0, 0.17 v5.3.0, 0.26 v5.2.0, 0.00 v5.0.0, 0.10 v4.1.0, 0.17 v4.0.1, 0.26 v4.0.0, 0.25 v3.7.0, 0.14 v3.5.0, 0.00 v3.2.0, 0.33 v3.1.0
% Syntax   : Number of formulae    :    1 (   0 unt;   0 def)
%            Number of atoms       :   16 (   0 equ)
%            Maximal formula atoms :   16 (  16 avg)
%            Number of connectives :   15 (   0   ~;   0   |;  10   &)
%                                         (   0 <=>;   5  =>;   0  <=;   0 <~>)
%            Maximal formula depth :   15 (  15 avg)
%            Maximal term depth    :    2 (   1 avg)
%            Number of predicates  :    1 (   1 usr;   0 prp; 3-3 aty)
%            Number of functors    :    1 (   1 usr;   0 con; 1-1 aty)
%            Number of variables   :   24 (  23   !;   1   ?)
% SPC      : FOF_THM_RFO_NEQ

% Comments :
%------------------------------------------------------------------------------
fof(commutativity,conjecture,
    ! [E] :
      ( ( ! [X,Y] :
          ? [Z] : product(X,Y,Z)
        & ! [X,Y,Z,U,V,W] :
            ( ( product(X,Y,U)
              & product(Y,Z,V)
              & product(U,Z,W) )
           => product(X,V,W) )
        & ! [X,Y,Z,U,V,W] :
            ( ( product(X,Y,U)
              & product(Y,Z,V)
              & product(X,V,W) )
           => product(U,Z,W) )
        & ! [X] : product(X,E,X)
        & ! [X] : product(E,X,X)
        & ! [X] : product(X,inverse(X),E)
        & ! [X] : product(inverse(X),X,E) )
     => ( ! [X] : product(X,X,E)
       => ! [U,V,W] :
            ( product(U,V,W)
           => product(V,U,W) ) ) ) ).

%------------------------------------------------------------------------------

