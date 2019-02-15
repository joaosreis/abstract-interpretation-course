open Abstract_syntax_tree
open Cfg

module D = My_domain

let x = {
  var_id = 0;
  var_name = "x";
  var_type = AST_TYP_INT;
  var_pos = extent_unknown
}

let y = {
  var_id = 1;
  var_name = "y";
  var_type = AST_TYP_INT;
  var_pos = extent_unknown
}

let z = {
  var_id = 2;
  var_name = "z";
  var_type = AST_TYP_INT;
  var_pos = extent_unknown
}

let d =
  let assign v e t = D.assign t v e in
  assign x (CFG_int_rand (Z.zero, Z.of_int 10)) D.bottom |>
  assign y (CFG_int_rand (Z.of_int 2, Z.of_int 10)) |>
  assign z (CFG_int_rand (Z.of_int 3, Z.of_int 5))

let e_1 =
  CFG_int_binary (AST_MINUS,
                  (CFG_int_binary (AST_PLUS, CFG_int_var x, CFG_int_var y)),
                  (CFG_int_var z))

let e_2 = CFG_int_const Z.zero

let expr =
  CFG_compare (AST_LESS_EQUAL, e_1, e_2)

let () = D.print stdout (D.guard d expr); print_newline ()
