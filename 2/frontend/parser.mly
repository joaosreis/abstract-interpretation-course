(*
  Cours "Semantics and applications to verification"

  Antoine Miné 2014
  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)


(*
  Parser for a very simple C-like "curly bracket" language.
*)

%{
open Abstract_syntax_tree
open Abstract_syntax_utils
%}

/* tokens */
/**********/

%token TOK_TRUE
%token TOK_FALSE
%token TOK_WHILE
%token TOK_IF
%token TOK_ELSE
%token TOK_HALT
%token TOK_RAND
%token TOK_ASSERT
%token TOK_PRINT
%token TOK_VAR

%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_LCURLY
%token TOK_RCURLY
%token TOK_STAR
%token TOK_PLUS
%token TOK_MINUS
%token TOK_EXCLAIM
%token TOK_DIVIDE
%token TOK_PERCENT
%token TOK_LESS
%token TOK_GREATER
%token TOK_LESS_EQUAL
%token TOK_GREATER_EQUAL
%token TOK_EQUAL_EQUAL
%token TOK_NOT_EQUAL
%token TOK_AND_AND
%token TOK_BAR_BAR
%token TOK_SEMICOLON
%token TOK_COMMA
%token TOK_EQUAL

%token <string> TOK_id
%token <string> TOK_int

%token TOK_EOF

/* priorities of binary operators (lowest to highest) */
%left TOK_BAR_BAR
%left TOK_AND_AND
%left TOK_EQUAL_EQUAL TOK_NOT_EQUAL
%left TOK_LESS TOK_GREATER TOK_LESS_EQUAL TOK_GREATER_EQUAL
%left TOK_PLUS TOK_MINUS
%left TOK_STAR TOK_DIVIDE TOK_PERCENT


/* entry-points */
/****************/

%start<Abstract_syntax_tree.prog> file


%%


/* toplevel */
/************/

file: t=ext(list(toplevel)) TOK_EOF { t }

toplevel:
| d=ext(stat)           { AST_stat d }


/* expressions */
/***************/

primary_expr:
| TOK_LPAREN e=expr TOK_RPAREN     { e }
| e=ext(TOK_id)                    { let name, ext = e in AST_variable (make_dummy_variable name, ext) }
| e=ext(TOK_int)                   { AST_int_const (e |> fst |> Z.of_string) }
| TOK_TRUE                         { AST_bool_const true }
| TOK_FALSE                        { AST_bool_const false }
| TOK_RAND TOK_LPAREN e1=ext(sign_int_literal)
           TOK_COMMA  e2=ext(sign_int_literal) TOK_RPAREN
  { let i1, ext1 = e1 in
    let i2, ext2 = e2 in
    AST_int_rand ((i1, ext1), (i2, ext2))
  }


/* integer with optional sign */
sign_int_literal:
| i=TOK_int            { Z.of_string i }
| TOK_PLUS i=TOK_int   { Z.of_string i }
| TOK_MINUS i=TOK_int  { i |> Z.of_string |> Z.(~-) }


unary_expr:
| e=primary_expr                   { e }
| o=unary_op e=ext(unary_expr)     { AST_unary (o, e) }

%inline unary_op:
| TOK_PLUS           { AST_UNARY_PLUS }
| TOK_MINUS          { AST_UNARY_MINUS }
| TOK_EXCLAIM        { AST_NOT }


binary_expr:
| e=unary_expr                                        { e }
| e=ext(binary_expr) o=binary_op f=ext(binary_expr)   { AST_binary (o, e, f) }

%inline binary_op:
| TOK_STAR           { AST_MULTIPLY }
| TOK_DIVIDE         { AST_DIVIDE }
| TOK_PERCENT        { AST_MODULO }
| TOK_PLUS           { AST_PLUS }
| TOK_MINUS          { AST_MINUS }
| TOK_LESS           { AST_LESS }
| TOK_GREATER        { AST_GREATER }
| TOK_LESS_EQUAL     { AST_LESS_EQUAL }
| TOK_GREATER_EQUAL  { AST_GREATER_EQUAL }
| TOK_EQUAL_EQUAL    { AST_EQUAL }
| TOK_NOT_EQUAL      { AST_NOT_EQUAL }
| TOK_AND_AND        { AST_AND }
| TOK_BAR_BAR        { AST_OR }

expr:
| e=binary_expr { e }

lvalue:
| i=TOK_id   { make_dummy_variable i }

/* declarations */
/****************/

var_decl:
| TOK_VAR i=separated_list(TOK_COMMA, init_declarator) TOK_SEMICOLON { i }

init_declarator:
| v=ext(TOK_id)                         { let name, ext = v in (make_dummy_variable name, ext), None }
| v=ext(TOK_id) TOK_EQUAL i=ext(expr)   { let name, ext = v in (make_dummy_variable name, ext), Some i }

/* statements */
/**************/

block:
| TOK_LCURLY l=list(ext(stat)) TOK_RCURLY  { l }

common_stat:
| l=block { AST_block l }
| e=ext(lvalue) TOK_EQUAL f=ext(expr) TOK_SEMICOLON { AST_assign (e, f) }
| TOK_ASSERT TOK_LPAREN e=ext(expr) TOK_RPAREN TOK_SEMICOLON { AST_assert e }
| TOK_PRINT TOK_LPAREN l=separated_list(TOK_COMMA,ext(lvalue)) TOK_RPAREN TOK_SEMICOLON { AST_print l }
| TOK_HALT TOK_SEMICOLON { AST_HALT }
| v=var_decl { AST_local v }

stat_with_else:
| s=common_stat { s }
| TOK_IF TOK_LPAREN e=ext(expr) TOK_RPAREN s=ext(stat_with_else) TOK_ELSE t=ext(stat_with_else) { AST_if (e, s, Some t) }
| TOK_WHILE TOK_LPAREN e=ext(expr) TOK_RPAREN s=ext(stat_with_else) { AST_while (e, s) }

stat:
| s=common_stat { s }
| TOK_IF TOK_LPAREN e=ext(expr) TOK_RPAREN s=ext(stat) { AST_if (e, s, None) }
| TOK_IF TOK_LPAREN e=ext(expr) TOK_RPAREN s=ext(stat_with_else) TOK_ELSE t=ext(stat) { AST_if (e, s, Some t) }
| TOK_WHILE TOK_LPAREN e=ext(expr) TOK_RPAREN s=ext(stat) { AST_while (e, s) }


/* utilities */
/*************/

/* adds extent information to rule */
%inline ext(X):
| x=X { x, ($startpos, $endpos) }


%%
