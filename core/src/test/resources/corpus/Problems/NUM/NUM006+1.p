
%------------------------------------------------------------------------------
% File     : NUM006+1 : TPTP v9.3.1. Released v7.3.0.
% Domain   : Number Theory
% Problem  : Robinson arithmetic: Goldbach conjecture
% Version  : Especial.
% English  :

% Refs     : [BBJ03] Boolos et al. (2003), Computability and Logic
%          : [Smi07] Smith (2007), An Introduction to Goedel's Theorems
%          : [Lam18] Lampert (2018), Email to Geoff Sutcliffe
% Source   : [Lam18]
% Names    : goldbachid [Lam18]

% Status   : Open
% Rating   : 1.00 v7.3.0
% Syntax   : Number of formulae    :   19 (   1 unt;   0 def)
%            Number of atoms       :  118 (   0 equ)
%            Maximal formula atoms :   43 (   6 avg)
%            Number of connectives :  156 (  57   ~;  51   |;  48   &)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%            Maximal formula depth :   19 (   8 avg)
%            Maximal term depth    :    1 (   1 avg)
%            Number of predicates  :    5 (   5 usr;   0 prp; 1-3 aty)
%            Number of functors    :    0 (   0 usr;   0 con; --- aty)
%            Number of variables   :  106 (  71   !;  35   ?)
% SPC      : FOF_OPN_RFO_NEQ

% Comments : Translated to FOL without equality.
%------------------------------------------------------------------------------
include('Axioms/NUM009+0.ax').
%------------------------------------------------------------------------------
fof(goldbachid,conjecture,
    ! [X1] :
      ( ! [Y1,Y3] :
          ( ! [Y17] :
              ( ~ id(Y17,X1)
              | ~ r3(Y1,Y3,Y17) )
          | ! [Y9] :
              ( ! [Y18] :
                  ( ! [Y20] :
                      ( ! [Y26] :
                          ( ~ r1(Y26)
                          | ~ r2(Y26,Y20) )
                      | ~ r2(Y20,Y18) )
                  | ~ r4(Y18,Y1,Y9) )
              | ~ id(Y9,X1) ) )
      | ? [Y19] :
          ( id(X1,Y19)
          & ? [Y21] :
              ( r2(Y21,Y19)
              & ? [Y27] :
                  ( r1(Y27)
                  & r2(Y27,Y21) ) ) )
      | ? [Y28] :
          ( id(X1,Y28)
          & r1(Y28) )
      | ? [Y2,Y4] :
          ( ! [X2,X4] :
              ( ! [Y14] :
                  ( ~ id(Y14,Y2)
                  | ~ r4(X2,X4,Y14) )
              | ! [Y5,Y16] :
                  ( ~ id(Y16,Y2)
                  | ~ r3(X2,Y5,Y16) )
              | ! [Y6,Y15] :
                  ( ~ id(Y15,Y2)
                  | ~ r3(X4,Y6,Y15) )
              | ? [Y22] :
                  ( id(X2,Y22)
                  & ? [Y29] :
                      ( r1(Y29)
                      & r2(Y29,Y22) ) )
              | ? [Y23] :
                  ( id(X4,Y23)
                  & ? [Y30] :
                      ( r1(Y30)
                      & r2(Y30,Y23) ) ) )
          & ! [X3,X5] :
              ( ! [Y11] :
                  ( ~ id(Y11,Y4)
                  | ~ r4(X3,X5,Y11) )
              | ! [Y7,Y13] :
                  ( ~ id(Y13,Y4)
                  | ~ r3(X3,Y7,Y13) )
              | ! [Y8,Y12] :
                  ( ~ id(Y12,Y4)
                  | ~ r3(X5,Y8,Y12) )
              | ? [Y24] :
                  ( id(X3,Y24)
                  & ? [Y31] :
                      ( r1(Y31)
                      & r2(Y31,Y24) ) )
              | ? [Y25] :
                  ( id(X5,Y25)
                  & ? [Y32] :
                      ( r1(Y32)
                      & r2(Y32,Y25) ) ) )
          & ! [Y33] :
              ( ~ id(Y2,Y33)
              | ~ r1(Y33) )
          & ! [Y34] :
              ( ~ id(Y4,Y34)
              | ~ r1(Y34) )
          & ? [Y10] :
              ( id(Y10,X1)
              & r3(Y2,Y4,Y10) ) ) ) ).

%------------------------------------------------------------------------------

