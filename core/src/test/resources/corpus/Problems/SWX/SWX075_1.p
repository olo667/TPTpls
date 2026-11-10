%------------------------------------------------------------------------------
% File     : SWX075_1 : TPTP v9.3.1. Released v9.1.0.
% Domain   : Software Verification
% Problem  : Anthem problem formula_6_unnamed_formula 
% Version  : Especial.
% English  :

% Refs     : [FL+20] Fandinno et al. (2020), Verifying Tight Logic Programs
%          : [FH+23] Fandinno et al. (2023), External Behavior of a Logic P
%          : [Han25] Hansen (2025), Email to Geoff Sutcliffe
% Source   : [Han25]
% Names    :

% Status   : Theorem
% Rating   : 0.60 v9.3.0, 0.75 v9.1.0
% Syntax   : Number of formulae    :   39 (   3 unt;  17 typ;   0 def)
%            Number of atoms       :   81 (  35 equ)
%            Maximal formula atoms :   10 (   3 avg)
%            Number of connectives :   65 (   6   ~;   4   |;  38   &)
%                                         (  10 <=>;   7  =>;   0  <=;   0 <~>)
%            Maximal formula depth :   12 (   6 avg)
%            Maximal term depth    :    2 (   1 avg)
%            Number arithmetic     :    8 (   1 atm;   0 fun;   0 num;   7 var)
%            Number of types       :    4 (   2 usr;   1 ari;   0 dat;   0 cdt)
%            Number of type conns  :   19 (  13   >;   6   *;   0   +;   0  <<)
%            Number of predicates  :   14 (  11 usr;   1 prp; 0-2 aty)
%            Number of functors    :    4 (   4 usr;   2 con; 0-1 aty)
%            Number of variables   :   66 (  40   !;  26   ?;  66   :)
% SPC      : TF0_THM_EQU_ARI_NDT

% Comments : From https://github.com/ZachJHansen/anthem-benchmarks/tree/tptp
%------------------------------------------------------------------------------
include('Axioms/SWV014_0.ax').
%------------------------------------------------------------------------------
tff(predicate_0,type,
    edge: ( general * general ) > $o ).

tff(predicate_1,type,
    vertex: general > $o ).

tff(predicate_2,type,
    aux: general > $o ).

tff(predicate_3,type,
    color: general > $o ).

tff(predicate_4,type,
    color: ( general * general ) > $o ).

tff(formula_0_unnamed_formula,axiom,
    ! [X_g: general,Y_g: general] :
      ( edge(X_g,Y_g)
     => ( vertex(X_g)
        & vertex(Y_g) ) ) ).

tff(formula_1_completed_definition_of_aux_1,axiom,
    ! [V1_g: general] :
      ( aux(V1_g)
    <=> ? [X_g: general,Z_g: general] :
          ( ( V1_g = X_g )
          & ? [Z_g: general] :
              ( ( Z_g = X_g )
              & vertex(Z_g) )
          & ? [Z1_g: general] :
              ( ( Z1_g = Z_g )
              & color(Z1_g) )
          & ? [Z1_g: general,Z2_g: general] :
              ( ( Z1_g = X_g )
              & ( Z2_g = Z_g )
              & color(Z1_g,Z2_g) ) ) ) ).

tff(formula_2_constraint_0,axiom,
    ! [X_g: general,Z1_g: general,Z2_g: general] :
      ( ( ? [Z_g: general,Z2_g: general] :
            ( ( Z_g = X_g )
            & ( Z2_g = Z1_g )
            & color(Z_g,Z2_g) )
        & ? [Z_g: general,Z1_g: general] :
            ( ( Z_g = X_g )
            & ( Z1_g = Z2_g )
            & color(Z_g,Z1_g) )
        & ? [Z_g: general,Z3_g: general] :
            ( ( Z_g = Z1_g )
            & ( Z3_g = Z2_g )
            & ( Z_g != Z3_g ) ) )
     => $false ) ).

tff(formula_3_constraint_1,axiom,
    ! [X_g: general] :
      ( ( ? [Z_g: general] :
            ( ( Z_g = X_g )
            & vertex(Z_g) )
        & ? [Z_g: general] :
            ( ( Z_g = X_g )
            & ~ aux(Z_g) ) )
     => $false ) ).

tff(formula_4_constraint_2,axiom,
    ! [X_g: general,Y_g: general,Z_g: general] :
      ( ( ? [Z_g: general,Z1_g: general] :
            ( ( Z_g = X_g )
            & ( Z1_g = Y_g )
            & edge(Z_g,Z1_g) )
        & ? [Z1_g: general,Z2_g: general] :
            ( ( Z1_g = X_g )
            & ( Z2_g = Z_g )
            & color(Z1_g,Z2_g) )
        & ? [Z1_g: general,Z2_g: general] :
            ( ( Z1_g = Y_g )
            & ( Z2_g = Z_g )
            & color(Z1_g,Z2_g) ) )
     => $false ) ).

tff(formula_5_completed_definition_of_color_2,axiom,
    ! [V1_g: general,V2_g: general] :
      ( color(V1_g,V2_g)
    <=> ? [X_g: general,Z_g: general] :
          ( ( V1_g = X_g )
          & ( V2_g = Z_g )
          & ? [Z_g: general] :
              ( ( Z_g = X_g )
              & vertex(Z_g) )
          & ? [Z1_g: general] :
              ( ( Z1_g = Z_g )
              & color(Z1_g) )
          & ~ ~ color(V1_g,V2_g) ) ) ).

tff(formula_6_unnamed_formula,conjecture,
    ! [X_g: general,Z_g: general] :
      ( color(X_g,Z_g)
     => ( vertex(X_g)
        & color(Z_g) ) ) ).

%------------------------------------------------------------------------------
