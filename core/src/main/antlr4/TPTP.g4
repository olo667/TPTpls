grammar TPTP;

// Generated from SyntaxBNF-v9.3.1.4 using BNFMetaParser.g4.
// ::= rules are parser rules; :== restrictions are comments;
// ::- rules are tokens; ::: rules are lexer fragments unless
// referenced directly by a parser rule.

tptp_file
    : tptp_input* EOF
    ;

tptp_input
    : annotated_formula
    | include
    ;

annotated_formula
    : thf_annotated
    | tff_annotated
    | tcf_annotated
    | fof_annotated
    | cnf_annotated
    | tpi_annotated
    ;

tpi_annotated
    : 'tpi' '(' name ',' formula_role ',' tpi_formula annotations ')' '.'
    ;

tpi_formula
    : fof_formula
    ;

thf_annotated
    : 'thf' '(' name ',' formula_role ',' thf_formula annotations ')' '.'
    ;

tff_annotated
    : 'tff' '(' name ',' formula_role ',' tff_formula annotations ')' '.'
    ;

tcf_annotated
    : 'tcf' '(' name ',' formula_role ',' tcf_formula annotations ')' '.'
    ;

fof_annotated
    : 'fof' '(' name ',' formula_role ',' fof_formula annotations ')' '.'
    ;

cnf_annotated
    : 'cnf' '(' name ',' formula_role ',' cnf_formula annotations ')' '.'
    ;

annotations
    : ',' source optional_info
    | nothing
    ;

formula_role
    : Lower_word
    | Lower_word '-' general_term
    ;

thf_formula
    : thf_logic_formula
    | thf_atom_typing
    | thf_subtype
    ;

thf_logic_formula
    : thf_unitary_formula
    | thf_unary_formula
    | thf_binary_formula
    | thf_defined_infix
    | thf_definition
    | thf_sequent
    ;

thf_binary_formula
    : thf_binary_pair
    | thf_binary_list
    | thf_binary_type
    ;

thf_binary_pair
    : thf_unit_formula nonassoc_connective thf_unit_formula
    ;

thf_binary_list
    : thf_or_formula
    | thf_and_formula
    | thf_apply_formula
    ;

thf_or_formula
    : thf_unit_formula Vline thf_unit_formula
    | thf_or_formula Vline thf_unit_formula
    ;

thf_and_formula
    : thf_unit_formula '&' thf_unit_formula
    | thf_and_formula '&' thf_unit_formula
    ;

thf_apply_formula
    : thf_unit_formula '@' thf_unit_formula
    | thf_apply_formula '@' thf_unit_formula
    ;

thf_unit_formula
    : thf_unitary_formula
    | thf_unary_formula
    | thf_defined_infix
    ;

thf_preunit_formula
    : thf_unitary_formula
    | thf_prefix_unary
    ;

thf_unitary_formula
    : thf_quantified_formula
    | thf_atomic_formula
    | variable
    | '(' thf_logic_formula ')'
    ;

thf_quantified_formula
    : thf_quantification thf_unit_formula
    ;

thf_quantification
    : thf_quantifier '[' thf_variable_list ']' ':'
    ;

thf_variable_list
    : thf_typed_variable
    | thf_typed_variable ',' thf_variable_list
    ;

thf_typed_variable
    : variable ':' thf_top_level_type
    ;

thf_unary_formula
    : thf_prefix_unary
    | thf_infix_unary
    ;

thf_prefix_unary
    : thf_unary_connective thf_preunit_formula
    ;

thf_infix_unary
    : thf_unitary_term infix_inequality thf_unitary_term
    ;

thf_atomic_formula
    : thf_plain_atomic
    | thf_defined_atomic
    | thf_system_atomic
    | thf_fof_function
    ;

thf_plain_atomic
    : constant
    | thf_tuple
    ;

thf_defined_atomic
    : defined_constant
    | thf_defined_term
    | '(' thf_conn_term ')'
    | nhf_long_connective
    | thf_let
    ;

thf_defined_term
    : defined_term
    | th1_defined_term
    ;

thf_defined_infix
    : thf_unitary_term defined_infix_pred thf_unitary_term
    ;

thf_system_atomic
    : system_constant
    ;

thf_let
    : '$let' '(' thf_let_types ',' thf_let_defns ',' thf_logic_formula ')'
    ;

thf_let_types
    : thf_atom_typing
    | '[' thf_atom_typing_list ']'
    ;

thf_atom_typing_list
    : thf_atom_typing
    | thf_atom_typing ',' thf_atom_typing_list
    ;

thf_let_defns
    : thf_let_defn
    | '[' thf_let_defn_list ']'
    ;

thf_let_defn
    : thf_logic_formula assignment thf_logic_formula
    ;

thf_let_defn_list
    : thf_let_defn
    | thf_let_defn ',' thf_let_defn_list
    ;

thf_unitary_term
    : thf_atomic_formula
    | variable
    | '(' thf_logic_formula ')'
    ;

thf_conn_term
    : nonassoc_connective
    | assoc_connective
    | infix_equality
    | infix_inequality
    | thf_unary_connective
    ;

thf_tuple
    : '[' ']'
    | '[' thf_formula_list ']'
    ;

thf_fof_function
    : defined_functor '(' thf_arguments ')'
    | system_functor '(' thf_arguments ')'
    | functor '(' thf_arguments ')'
    ;

thf_arguments
    : thf_formula_list
    ;

thf_formula_list
    : thf_logic_formula comma_thf_logic_formula*
    ;

comma_thf_logic_formula
    : ',' thf_logic_formula
    ;

thf_atom_typing
    : typeable_atom ':' thf_top_level_type
    | '(' thf_atom_typing ')'
    ;

thf_top_level_type
    : thf_unitary_type
    | thf_mapping_type
    | thf_apply_type
    ;

thf_unitary_type
    : thf_unitary_formula
    ;

thf_apply_type
    : thf_apply_formula
    ;

thf_binary_type
    : thf_mapping_type
    | thf_xprod_type
    | thf_union_type
    ;

thf_mapping_type
    : thf_unitary_type Arrow thf_unitary_type
    | thf_unitary_type Arrow thf_mapping_type
    ;

thf_xprod_type
    : thf_unitary_type Star thf_unitary_type
    | thf_xprod_type Star thf_unitary_type
    ;

thf_union_type
    : thf_unitary_type Plus thf_unitary_type
    | thf_union_type Plus thf_unitary_type
    ;

thf_subtype
    : atomic_type subtype_sign atomic_type
    ;

thf_definition
    : thf_atomic_formula identical thf_logic_formula
    ;

thf_sequent
    : thf_tuple gentzen_arrow thf_tuple
    ;

tff_formula
    : tff_logic_formula
    | tff_atom_typing
    | tff_subtype
    ;

tff_logic_formula
    : tff_unitary_formula
    | tff_unary_formula
    | tff_binary_formula
    | tff_defined_infix
    | txf_definition
    | txf_sequent
    ;

tff_binary_formula
    : tff_binary_pair
    | tff_binary_list
    ;

tff_binary_pair
    : tff_unit_formula nonassoc_connective tff_unit_formula
    ;

tff_binary_list
    : tff_or_formula
    | tff_and_formula
    ;

tff_or_formula
    : tff_unit_formula Vline tff_unit_formula
    | tff_or_formula Vline tff_unit_formula
    ;

tff_and_formula
    : tff_unit_formula '&' tff_unit_formula
    | tff_and_formula '&' tff_unit_formula
    ;

tff_unit_formula
    : tff_unitary_formula
    | tff_unary_formula
    | tff_defined_infix
    ;

tff_preunit_formula
    : tff_unitary_formula
    | tff_prefix_unary
    ;

tff_unitary_formula
    : tff_quantified_formula
    | tff_atomic_formula
    | txf_unitary_formula
    | '(' tff_logic_formula ')'
    ;

txf_unitary_formula
    : variable
    ;

tff_quantified_formula
    : tff_quantifier '[' tff_variable_list ']' ':' tff_unit_formula
    ;

tff_variable_list
    : tff_variable
    | tff_variable ',' tff_variable_list
    ;

tff_variable
    : tff_typed_variable
    | variable
    ;

tff_typed_variable
    : variable ':' tff_atomic_type
    ;

tff_unary_formula
    : tff_prefix_unary
    | tff_infix_unary
    ;

tff_prefix_unary
    : tff_unary_connective tff_preunit_formula
    ;

tff_infix_unary
    : tff_unitary_term infix_inequality tff_unitary_term
    ;

tff_atomic_formula
    : tff_plain_atomic
    | tff_defined_atomic
    | tff_system_atomic
    ;

tff_plain_atomic
    : constant
    | functor '(' tff_arguments ')'
    ;

tff_defined_atomic
    : tff_defined_plain
    ;

tff_defined_plain
    : defined_constant
    | defined_functor '(' tff_arguments ')'
    | nxf_atom
    | txf_let
    ;

tff_defined_infix
    : tff_unitary_term defined_infix_pred tff_unitary_term
    ;

tff_system_atomic
    : system_constant
    | system_functor '(' tff_arguments ')'
    ;

txf_let
    : '$let' '(' txf_let_types ',' txf_let_defns ',' tff_term ')'
    ;

txf_let_types
    : tff_atom_typing
    | '[' tff_atom_typing_list ']'
    ;

tff_atom_typing_list
    : tff_atom_typing
    | tff_atom_typing ',' tff_atom_typing_list
    ;

txf_let_defns
    : txf_let_defn
    | '[' txf_let_defn_list ']'
    ;

txf_let_defn
    : txf_let_lhs assignment tff_term
    ;

txf_let_lhs
    : tff_plain_atomic
    | txf_tuple
    ;

txf_let_defn_list
    : txf_let_defn
    | txf_let_defn ',' txf_let_defn_list
    ;

nxf_atom
    : nxf_long_connective '@' '(' tff_arguments ')'
    ;

tff_term
    : tff_logic_formula
    | defined_term
    | txf_tuple
    ;

tff_unitary_term
    : tff_atomic_formula
    | defined_term
    | txf_tuple
    | variable
    | '(' tff_logic_formula ')'
    ;

txf_tuple
    : '[' ']'
    | '[' tff_arguments ']'
    ;

tff_arguments
    : tff_term comma_tff_term*
    ;

comma_tff_term
    : ',' tff_term
    ;

tff_atom_typing
    : typeable_atom ':' tff_top_level_type
    | '(' tff_atom_typing ')'
    ;

tff_top_level_type
    : tff_atomic_type
    | tff_non_atomic_type
    ;

tff_non_atomic_type
    : tff_mapping_type
    | tf1_quantified_type
    | '(' tff_non_atomic_type ')'
    ;

tf1_quantified_type
    : type_quantifier '[' tff_variable_list ']' ':' tff_monotype
    ;

tff_monotype
    : tff_atomic_type
    | '(' tff_mapping_type ')'
    | tf1_quantified_type
    ;

tff_unitary_type
    : tff_atomic_type
    | '(' tff_xprod_type ')'
    ;

tff_atomic_type
    : type_constant
    | defined_type
    | variable
    | type_functor '(' tff_type_arguments ')'
    | '(' tff_atomic_type ')'
    | txf_tuple_type
    ;

tff_type_arguments
    : tff_atomic_type
    | tff_atomic_type ',' tff_type_arguments
    ;

tff_mapping_type
    : tff_unitary_type Arrow tff_atomic_type
    ;

tff_xprod_type
    : tff_unitary_type Star tff_atomic_type
    | tff_xprod_type Star tff_atomic_type
    ;

txf_tuple_type
    : '[' tff_type_list ']'
    ;

tff_type_list
    : tff_top_level_type
    | tff_top_level_type ',' tff_type_list
    ;

tff_subtype
    : atomic_type subtype_sign atomic_type
    ;

txf_definition
    : tff_atomic_formula identical tff_term
    ;

txf_sequent
    : txf_tuple gentzen_arrow txf_tuple
    ;

nhf_long_connective
    : '{' ntf_connective_name '}'
    | '{' ntf_connective_name '(' nhf_parameter_list ')' '}'
    ;

nhf_parameter_list
    : nhf_parameter
    | nhf_parameter ',' nhf_parameter_list
    ;

nhf_parameter
    : ntf_index
    | nhf_key_pair
    ;

nhf_key_pair
    : thf_definition
    ;

nxf_long_connective
    : '{' ntf_connective_name '}'
    | '{' ntf_connective_name '(' nxf_parameter_list ')' '}'
    ;

nxf_parameter_list
    : nxf_parameter
    | nxf_parameter ',' nxf_parameter_list
    ;

nxf_parameter
    : ntf_index
    | nxf_key_pair
    ;

nxf_key_pair
    : txf_definition
    ;

ntf_connective_name
    : ntf_defined_connective
    | atomic_system_word
    ;

ntf_defined_connective
    : atomic_defined_word
    ;

ntf_index
    : Hash tff_unitary_term
    ;

ntf_short_connective
    : '[' '.' ']'
    | Less_sign '.' Arrow
    | '{' '.' '}'
    | '(' '.' ')'
    ;

tcf_formula
    : tcf_logic_formula
    | tff_atom_typing
    ;

tcf_logic_formula
    : tcf_quantified_formula
    | cnf_formula
    ;

tcf_quantified_formula
    : '!' '[' tff_variable_list ']' ':' tcf_logic_formula
    ;

fof_formula
    : fof_logic_formula
    | fof_sequent
    ;

fof_logic_formula
    : fof_binary_formula
    | fof_unary_formula
    | fof_unitary_formula
    ;

fof_binary_formula
    : fof_binary_pair
    | fof_binary_list
    ;

fof_binary_pair
    : fof_unit_formula nonassoc_connective fof_unit_formula
    ;

fof_binary_list
    : fof_or_formula
    | fof_and_formula
    ;

fof_or_formula
    : fof_unit_formula Vline fof_unit_formula
    | fof_or_formula Vline fof_unit_formula
    ;

fof_and_formula
    : fof_unit_formula '&' fof_unit_formula
    | fof_and_formula '&' fof_unit_formula
    ;

fof_unary_formula
    : unary_connective fof_unit_formula
    | fof_infix_unary
    ;

fof_infix_unary
    : fof_term infix_inequality fof_term
    ;

fof_unit_formula
    : fof_unitary_formula
    | fof_unary_formula
    ;

fof_unitary_formula
    : fof_quantified_formula
    | fof_atomic_formula
    | '(' fof_logic_formula ')'
    ;

fof_quantified_formula
    : fof_quantifier '[' fof_variable_list ']' ':' fof_unit_formula
    ;

fof_variable_list
    : variable
    | variable ',' fof_variable_list
    ;

fof_atomic_formula
    : fof_plain_atomic_formula
    | fof_defined_atomic_formula
    | fof_system_atomic_formula
    ;

fof_plain_atomic_formula
    : fof_plain_term
    ;

fof_defined_atomic_formula
    : fof_defined_plain_formula
    | fof_defined_infix_formula
    ;

fof_defined_plain_formula
    : fof_defined_plain_term
    ;

fof_defined_infix_formula
    : fof_term defined_infix_pred fof_term
    ;

fof_system_atomic_formula
    : fof_system_term
    ;

fof_plain_term
    : constant
    | functor '(' fof_arguments ')'
    ;

fof_defined_term
    : defined_term
    | fof_defined_atomic_term
    ;

fof_defined_atomic_term
    : fof_defined_plain_term
    ;

fof_defined_plain_term
    : defined_constant
    | defined_functor '(' fof_arguments ')'
    ;

fof_system_term
    : system_constant
    | system_functor '(' fof_arguments ')'
    ;

fof_arguments
    : fof_term
    | fof_term ',' fof_arguments
    ;

fof_term
    : fof_function_term
    | variable
    ;

fof_function_term
    : fof_plain_term
    | fof_defined_term
    | fof_system_term
    ;

fof_sequent
    : fof_formula_tuple gentzen_arrow fof_formula_tuple
    | '(' fof_sequent ')'
    ;

fof_formula_tuple
    : '[' ']'
    | '[' fof_formula_tuple_list ']'
    ;

fof_formula_tuple_list
    : fof_logic_formula comma_fof_logic_formula*
    ;

comma_fof_logic_formula
    : ',' fof_logic_formula
    ;

cnf_formula
    : cnf_disjunction
    | '(' cnf_formula ')'
    ;

cnf_disjunction
    : cnf_literal
    | cnf_disjunction Vline cnf_literal
    ;

cnf_literal
    : fof_atomic_formula
    | '~' fof_atomic_formula
    | '~' '(' fof_atomic_formula ')'
    | fof_infix_unary
    ;

thf_quantifier
    : tff_quantifier
    | th0_quantifier
    | type_quantifier
    ;

thf_unary_connective
    : unary_connective
    | ntf_short_connective
    ;

th0_quantifier
    : '^'
    | '@+'
    | '@-'
    ;

type_quantifier
    : '!>'
    | '?*'
    ;

subtype_sign
    : '<<'
    ;

tff_unary_connective
    : unary_connective
    | ntf_short_connective
    ;

tff_quantifier
    : fof_quantifier
    | Hash
    ;

fof_quantifier
    : '!'
    | '?'
    ;

nonassoc_connective
    : '<=>'
    | '=>'
    | '<='
    | '<~>'
    | '~' Vline
    | '~&'
    ;

assoc_connective
    : Vline
    | '&'
    ;

unary_connective
    : '~'
    ;

gentzen_arrow
    : '-->'
    ;

assignment
    : ':='
    ;

identical
    : '=='
    ;

typeable_atom
    : constant
    | Distinct_object
    ;

atomic_type
    : typeable_atom
    | defined_constant
    | system_type
    ;

type_constant
    : type_functor
    ;

type_functor
    : atomic_word
    ;

defined_type
    : atomic_defined_word
    ;

system_type
    : atomic_system_word
    ;

defined_infix_pred
    : infix_equality
    ;

infix_equality
    : '='
    ;

infix_inequality
    : '!='
    ;

constant
    : functor
    ;

functor
    : atomic_word
    ;

defined_constant
    : defined_functor
    ;

defined_functor
    : atomic_defined_word
    ;

system_constant
    : system_functor
    ;

system_functor
    : atomic_system_word
    ;

th1_defined_term
    : '!!'
    | '??'
    | '@@+'
    | '@@-'
    | '@='
    ;

defined_term
    : number
    | Distinct_object
    ;

variable
    : Upper_word
    ;

source
    : dag_source
    | internal_source
    | external_source
    | '[' sources ']'
    ;

sources
    : source
    | source ',' sources
    ;

dag_source
    : name
    | inference_record
    ;

inference_record
    : 'inference' '(' inference_rule ',' useful_info ',' parents ')'
    ;

inference_rule
    : atomic_word
    ;

internal_source
    : 'introduced' '(' intro_type ',' useful_info ',' parents ')'
    ;

intro_type
    : atomic_word
    ;

external_source
    : file_source
    ;

file_source
    : 'file' '(' file_name file_info ')'
    ;

file_info
    : ',' name
    | nothing
    ;

parents
    : '[' ']'
    | '[' parent_list ']'
    ;

parent_list
    : parent_info comma_parent_info*
    ;

comma_parent_info
    : ',' parent_info
    ;

parent_info
    : source parent_details
    ;

parent_details
    : ':' general_term
    | nothing
    ;

optional_info
    : ',' useful_info
    | nothing
    ;

useful_info
    : general_list
    ;

include
    : 'include' '(' file_name include_optionals ')' '.'
    ;

include_optionals
    : nothing
    | ',' formula_selection
    | ',' formula_selection ',' space_name
    ;

formula_selection
    : '[' name_list ']'
    | Star
    ;

name_list
    : name
    | name ',' name_list
    ;

space_name
    : name
    ;

general_term
    : general_data
    | general_data ':' general_term
    | general_list
    ;

general_data
    : atomic_word
    | general_function
    | variable
    | number
    | Distinct_object
    | formula_data
    ;

general_function
    : atomic_word '(' general_terms ')'
    ;

formula_data
    : '$thf' '(' thf_formula ')'
    | '$tff' '(' tff_formula ')'
    | '$fof' '(' fof_formula ')'
    | '$cnf' '(' cnf_formula ')'
    | '$fot' '(' fof_term ')'
    ;

general_list
    : '[' ']'
    | '[' general_terms ']'
    ;

general_terms
    : general_term comma_general_term*
    ;

comma_general_term
    : ',' general_term
    ;

name
    : atomic_word
    | Integer
    ;

atomic_word
    : Lower_word
    | Single_quoted
    | Back_quoted
    ;

atomic_defined_word
    : Dollar_word
    ;

atomic_system_word
    : Dollar_dollar_word
    ;

number
    : Integer
    | Rational
    | Real
    ;

file_name
    : atomic_word
    ;

nothing
    :
    ;

// Semantic restrictions from SyntaxBNF.
// They constrain the language semantically and are intentionally
// not added as alternative context-free productions.
// line 79: <formula_role> :== axiom | hypothesis | definition | assumption | lemma | theorem | corollary | conjecture | negated_conjecture | plain | type | interpretation | logic | unknown
// line 181: <thf_unitary_type> :== <thf_atomic_type> | <th1_quantified_type>
// line 182: <thf_atomic_type> :== <type_constant> | <defined_type> | <variable> | <thf_mapping_type> | (<thf_atomic_type>)
// line 184: <th1_quantified_type> :== <type_quantifier> [<thf_variable_list>] : <thf_unitary_type>
// line 242: <tff_plain_atomic> :== <proposition> | <predicate>(<tff_arguments>)
// line 250: <tff_defined_plain> :== <defined_proposition> | <defined_predicate>(<tff_arguments>) | <nxf_atom> | <txf_conditional> | <txf_let>
// line 258: <tff_system_atomic> :== <system_proposition> | <system_predicate>(<tff_arguments>)
// line 260: <txf_conditional> :== $ite(<tff_logic_formula>,<tff_term>,<tff_term>)
// line 318: <ntf_connective_name> :== $box | $dia | {$necessary} | {$possible} | {$obligatory} | {$permissible} | {$knows} | {$canKnow} | {$believes} | {$canBelieve}
// line 328: <ntf_semantics_spec> :== <ntf_logic_name> <identical> [<ntf_logic_spec_list>]
// line 329: <ntf_logic_name> :== $modal | $alethic_modal | $deontic_modal | $epistemic_modal | $doxastic_modal | $temporal_instant
// line 331: <ntf_logic_spec_list> :== <ntf_logic_spec> | <ntf_logic_spec>,<ntf_logic_spec_list>
// line 332: <ntf_logic_spec> :== <ntf_domains_spec> | <ntf_designation_spec> | <ntf_terms_spec> | <ntf_modalities_spec> | <ntf_time_spec>
// line 334: <ntf_domains_spec> :== $domains <identical> <ntf_domains_value>
// line 335: <ntf_domains_value> :== <ntf_domain_type> | [<ntf_domain_type_list>]
// line 336: <ntf_domain_type> :== $constant | $varying | $cumulative | $decreasing | <tff_atomic_type> <identical> <ntf_domains_value>
// line 338: <ntf_domain_type_list> :== <ntf_domain_type> | <ntf_domain_type>,<ntf_domain_type_list>
// line 339: <ntf_designation_spec> :== $designation <identical> <ntf_designation_value>
// line 340: <ntf_designation_value> :== <ntf_designation_type> | [<ntf_designation_type_list>]
// line 341: <ntf_designation_type> :== $rigid | $flexible | <tff_atomic_type> <identical> <ntf_designation_value>
// line 343: <ntf_designation_type_list> :== <ntf_designation_type> | <ntf_designation_type>,<ntf_designation_type_list>
// line 345: <ntf_terms_spec> :== $terms <identical> <ntf_terms_value>
// line 346: <ntf_terms_value> :== <ntf_terms_type> | [<ntf_terms_type_list>]
// line 347: <ntf_terms_type> :== $local | $global | <tff_atomic_type> <identical> <ntf_terms_value>
// line 348: <ntf_terms_type_list> :== <ntf_terms_type> | <ntf_terms_type>,<ntf_terms_type_list>
// line 349: <ntf_modalities_spec> :== $modalities <identical> <ntf_modalities_value>
// line 350: <ntf_modalities_value> :== <ntf_modalities_type> | [<ntf_modalities_type_list>]
// line 351: <ntf_modalities_type> :== <ntf_modal_system> | <ntf_modal_axiom> | <tff_atomic_type> <identical> <ntf_modalities_value>
// line 353: <ntf_modalities_type_list> :== <ntf_modalities_type> | <ntf_modalities_type>,<ntf_modalities_type_list>
// line 355: <ntf_time_spec> :== $time <identical> <ntf_time_value>
// line 356: <ntf_time_value> :== <ntf_time_type> | [<ntf_time_type_list>]
// line 357: <ntf_time_type> :== $reflexivity | $irreflexivity | $transitivity | $asymmetry | $anti_symmetry | $linearity | $forward_linearity | $backward_linearity | $beginning | $end | $no_beginning | $no_end | $density | $forward_discreteness | $backward_discreteness | <tff_atomic_type> <identical> <ntf_time_value>
// line 362: <ntf_time_type_list> :== <ntf_time_type> | <ntf_time_type>,<ntf_time_type_list>
// line 364: <ntf_modal_system> :== $modal_system_K | $modal_system_KB | $modal_system_K4 | $modal_system_K5 | $modal_system_K45 | $modal_system_KB5 | $modal_system_D | $modal_system_DB | $modal_system_D4 | $modal_system_D5 | $modal_system_D45 | $modal_system_M | $modal_system_B | $modal_system_S4 | $modal_system_S5 | $modal_system_S5U
// line 370: <ntf_modal_axiom> :== $modal_axiom_K | $modal_axiom_M | $modal_axiom_B | $modal_axiom_D | $modal_axiom_4 | $modal_axiom_5 | $modal_axiom_CD | $modal_axiom_BoxM | $modal_axiom_C4 | $modal_axiom_C
// line 404: <fof_plain_atomic_formula> :== <proposition> | <predicate>(<fof_arguments>)
// line 407: <fof_defined_plain_formula> :== <defined_proposition> | <defined_predicate>(<fof_arguments>)
// line 478: <atomic_type> :== <type_constant> | <defined_type> | <system_type>
// line 488: <defined_type> :== $oType | $o | $iType | $i | $tType | $real | $rat | $int
// line 496: <proposition> :== <predicate>
// line 497: <predicate> :== <atomic_word>
// line 498: <defined_proposition> :== <defined_predicate>
// line 499: <defined_proposition> :== $true | $false
// line 500: <defined_predicate> :== <atomic_defined_word>
// line 501: <defined_predicate> :== $distinct | $less | $lesseq | $greater | $greatereq | $is_int | $is_rat
// line 507: <system_proposition> :== <system_predicate>
// line 508: <system_predicate> :== <atomic_system_word>
// line 516: <defined_functor> :== $uminus | $sum | $difference | $product | $quotient | $quotient_e | $quotient_t | $quotient_f | $remainder_e | $remainder_t | $remainder_f | $floor | $ceiling | $truncate | $round | $abs | $to_int | $to_rat | $to_real
// line 543: <intro_type> :== definition | tautology | assumption | theory
// line 560: <useful_info> :== [] | [<info_items>]
// line 561: <info_items> :== <info_item><comma_info_item>*
// line 562: <comma_info_item> :== ,<info_item>
// line 563: <info_item> :== <formula_item> | <inference_item> | <general_function>
// line 565: <formula_item> :== <description_item> | <iquote_item>
// line 566: <description_item> :== description(<atomic_word>)
// line 567: <iquote_item> :== iquote(<atomic_word>)
// line 572: <inference_item> :== <inference_status> | <assumptions_record> | <new_symbol_record> | <refutation>
// line 574: <inference_status> :== status(<status_value>) | <inference_info>
// line 590: <inference_info> :== <inference_rule>(<atomic_word>,<general_list>)
// line 593: <assumptions_record> :== assumptions([<name_list>])
// line 596: <refutation> :== refutation(<file_source>)
// line 598: <new_symbol_record> :== new_symbols(<atomic_word>,[<new_symbol_list>])
// line 599: <new_symbol_list> :== <principal_symbol> | <principal_symbol>,<new_symbol_list>
// line 601: <principal_symbol> :== <functor> | <variable>
// line 617: <general_data> :== bind(<variable>,<formula_data>) | bind_type(<variable>,<bound_type>)
// line 618: <bound_type> :== $thf(<thf_top_level_type>) | $tff(<tff_top_level_type>)

// Lexer rules derived from ::- and ::: definitions.
Single_quoted
    : Single_quote Sq_char Sq_char* Single_quote
    ;

Back_quoted
    : Back_quote Upper_word
    ;

Distinct_object
    : Double_quote Do_char Do_char* Double_quote
    ;

Dollar_word
    : Dollar Alpha_numeric*
    ;

Dollar_dollar_word
    : Dollar Dollar Alpha_numeric*
    ;

Upper_word
    : Upper_alpha Alpha_numeric*
    ;

Lower_word
    : Lower_alpha Alpha_numeric*
    ;

Real
    : ( Signed_real | Unsigned_real )
    ;

fragment Signed_real
    : Sign Unsigned_real
    ;

fragment Unsigned_real
    : ( Decimal_fraction | Decimal_exponent )
    ;

fragment Decimal_exponent
    : ( Integer_digits | Decimal_fraction ) Exponent Exp_integer
    ;

fragment Decimal_fraction
    : Unsigned_integer Dot Integer_digits
    ;

fragment Exp_integer
    : ( Signed_exp_integer | Integer_digits )
    ;

fragment Signed_exp_integer
    : Sign Integer_digits
    ;

Rational
    : ( Signed_rational | Unsigned_rational )
    ;

fragment Signed_rational
    : Sign Unsigned_rational
    ;

fragment Unsigned_rational
    : Unsigned_integer Slash Positive_integer
    ;

Integer
    : ( Signed_integer | Unsigned_integer )
    ;

fragment Signed_integer
    : Sign Unsigned_integer
    ;

fragment Unsigned_integer
    : ( Zero_numeric | Positive_integer )
    ;

fragment Positive_integer
    : Non_zero_numeric Numeric*
    ;

fragment Integer_digits
    : Numeric Numeric*
    ;

Slash
    : Slash_char
    ;

Slosh
    : Slosh_char
    ;

Vline
    : [|]
    ;

Star
    : [*]
    ;

Plus
    : [+]
    ;

Arrow
    : [>]
    ;

Less_sign
    : [<]
    ;

Hash
    : [#]
    ;

Comment
    : ( Comment_line | Comment_block )
    -> skip;

Comment_line
    : Percentage_sign ~[\r\n]*
    ;

fragment Comment_block
    : Slash_char Star Not_star_slash Star Star* Slash_char
    ;

fragment Not_star_slash
    : ( ~[*]* [*] [*]* ~[/*] )* ~[*]*
    ;

fragment Percentage_sign
    : [%]
    ;

fragment Double_quote
    : ["]
    ;

fragment Do_char
    : ( [\u0020-\u0021\u0023-\u005B\u005D-\u007E] | ( [\\] ["\\] ) )
    ;

fragment Single_quote
    : [']
    ;

fragment Back_quote
    : [`]
    ;

fragment Sq_char
    : ( [\u0020-\u0026\u0028-\u005B\u005D-\u007E] | ( [\\] ['\\] ) )
    ;

fragment Sign
    : [+\-]
    ;

fragment Dot
    : [.]
    ;

fragment Exponent
    : [Ee]
    ;

fragment Slash_char
    : [/]
    ;

fragment Slosh_char
    : [\\]
    ;

fragment Zero_numeric
    : [0]
    ;

fragment Non_zero_numeric
    : [1-9]
    ;

fragment Numeric
    : [0-9]
    ;

fragment Lower_alpha
    : [a-z]
    ;

fragment Upper_alpha
    : [A-Z]
    ;

fragment Underscore
    : [_]
    ;

fragment Alpha
    : ( Lower_alpha | Upper_alpha )
    ;

fragment Alpha_numeric
    : ( Lower_alpha | Upper_alpha | Numeric | Underscore )
    ;

fragment Dollar
    : [$]
    ;

fragment Printable_char
    : .
    ;

fragment Viewable_char
    : [.\n]
    ;

// SyntaxBNF permits whitespace between tokens.
WS
    : [ \t\r\n]+ -> skip
    ;
