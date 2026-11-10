%------------------------------------------------------------------------------
% File     : GRA076_1.002 : TPTP v9.3.1. Released v9.0.0.
% Domain   : Graph Theory
% Problem  : Adjacent vertices in a polygon with 2 black or white vertices
% Version  : Especial.
% English  : If a polygon with n black or white vertices, then two adjacent
%            vertices have the same color. If n is odd this is provable in
%            CPC.

% Refs     : [BHS00] Balsiger et al. (2000), A Benchmark Method for the Pro
%          : [NH+22] Nalon et al. (2022), Local Reductions for the Modal Cu
%          : [Nal22] Nalon (2022), Email to Geoff Sutcliffe
%          : [NH+23] Nalon et al. (2023), Buy One Get 14 Free: Evaluating L
% Source   : [Nal22]
% Names    : k_poly_p.0002 [Nal22]

% Status   : Theorem
% Rating   : 0.00 v9.0.0
% Syntax   : Number of formulae    :   15 (   0 unt;  14 typ;   0 def)
%            Number of atoms       :   38 (   0 equ)
%            Maximal formula atoms :   38 (  38 avg)
%            Number of connectives :  105 (  10   ~;  16   |;  14   &)
%                                         (   7 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (  58 {.};   0 {#})
%            Maximal formula depth :   22 (  22 avg)
%            Maximal term depth    :    0 (   0 avg)
%            Number of types       :    1 (   0 usr)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :   14 (  14 usr;  14 prp; 0-0 aty)
%            Number of functors    :    0 (   0 usr;   0 con; --- aty)
%            Number of variables   :    0 (   0   !;   0   ?;   0   :)
% SPC      : NX0_THM_PRP_NEQ_NAR_NDT

% Comments :
%------------------------------------------------------------------------------
tff('k_poly_p.0002',logic,
    $modal == 
      [ $modalities == $modal_system_K ] ).

tff(false_decl,type,
    false: $o ).

tff(p1_decl,type,
    p1: $o ).

tff(p10_decl,type,
    p10: $o ).

tff(p12_decl,type,
    p12: $o ).

tff(p14_decl,type,
    p14: $o ).

tff(p16_decl,type,
    p16: $o ).

tff(p2_decl,type,
    p2: $o ).

tff(p3_decl,type,
    p3: $o ).

tff(p4_decl,type,
    p4: $o ).

tff(p5_decl,type,
    p5: $o ).

tff(p6_decl,type,
    p6: $o ).

tff(p7_decl,type,
    p7: $o ).

tff(p8_decl,type,
    p8: $o ).

tff(p9_decl,type,
    p9: $o ).

tff(prove,conjecture,
    ~ ~ ( [.] [.] [.] [.] [.] [.] [.] [.] ( p1
                                          & p2
                                          & p3
                                          & p4
                                          & p5
                                          & p6
                                          & p7
                                          & p8 )
        | <.> ( <.> ( <.> ( <.> ( <.> ( <.> ( <.> ( false
                                                  | <.> ( p1
                                                      <=> p2 ) )
                                            | [.] p3
                                            | <.> <.> ( p2
                                                    <=> p3 ) )
                                      | [.] p4
                                      | <.> <.> <.> ( p3
                                                  <=> p4 ) )
                                | [.] p5
                                | <.> <.> <.> <.> ( p4
                                                <=> p5 ) )
                          | [.] p6
                          | <.> <.> <.> <.> <.> ( p5
                                              <=> p6 ) )
                    | [.] p7
                    | <.> <.> <.> <.> <.> <.> ( p6
                                            <=> p7 ) )
              | [.] p8
              | <.> <.> <.> <.> <.> <.> <.> ( p7
                                          <=> p1 ) )
        | [.] p9
        | [.] [.] [.] [.] [.] [.] [.] [.] ( ~ p2
                                          & ~ p4
                                          & ~ p6
                                          & ~ p8
                                          & ~ p10
                                          & ~ p12
                                          & ~ p14
                                          & ~ p16 ) ) ).

%------------------------------------------------------------------------------
