
(* This file was auto-generated based on "frontend/parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "Expected statement.\n"
    | 103 ->
        "Expected \";\".\n"
    | 28 ->
        "Expected \";\".\n"
    | 29 ->
        "Expected expression.\n"
    | 105 ->
        "Unexpected \")\".\n"
    | 32 ->
        "Expected \";\".\n"
    | 31 ->
        "Expected expression.\n"
    | 33 ->
        "Expected expression.\n"
    | 38 ->
        "Expected \";\".\n"
    | 37 ->
        "Expected expression.\n"
    | 40 ->
        "Expected \";\".\n"
    | 39 ->
        "Expected expression.\n"
    | 42 ->
        "Expected \";\".\n"
    | 41 ->
        "Expected expression\n"
    | 44 ->
        "Expected \";\".\n"
    | 43 ->
        "Expected expression.\n"
    | 46 ->
        "Expected \";\".\n"
    | 45 ->
        "Expected expression.\n"
    | 48 ->
        "Expected \";\".\n"
    | 47 ->
        "Expected expression.\n"
    | 50 ->
        "Unexpected exprssion.\n"
    | 49 ->
        "Expected expression.\n"
    | 35 ->
        "Expected expression.\n"
    | 52 ->
        "Expected \";\".\n"
    | 51 ->
        "Expected expression.\n"
    | 54 ->
        "Expected \";\".\n"
    | 53 ->
        "Expected expression.\n"
    | 104 ->
        "Expected expression.\n"
    | 7 ->
        "Expected \"(\" after rand keyword.\n"
    | 14 ->
        "Expected \";\".\n"
    | 16 ->
        "Expected \";\".\n"
    | 15 ->
        "Expected literal value.\n"
    | 8 ->
        "Expected literal value.\n"
    | 10 ->
        "Expected literal value.\n"
    | 12 ->
        "Expected literal value.\n"
    | 18 ->
        "Expected expression.\n"
    | 19 ->
        "Expected expression.\n"
    | 26 ->
        "Expected \")\".\n"
    | 20 ->
        "Expected expression.\n"
    | 22 ->
        "Expected expression.\n"
    | 2 ->
        "Expected \"(\" after \"while\" keyword.\n"
    | 57 ->
        "Expected \")\".\n"
    | 58 ->
        "Expected statement.\n"
    | 3 ->
        "Expected expression in while condition.\n"
    | 69 ->
        "Expected \"(\" after print statement.\n"
    | 70 ->
        "Expected lvalue in print statement.\n"
    | 72 ->
        "Expected \",\" between each lvalue in print statement.\n"
    | 73 ->
        "Expected lvalue in print statement.\n"
    | 76 ->
        "Expected \";\" after print statement.\n"
    | 78 ->
        "Expected statement.\n"
    | 119 ->
        "Expected statement.\n"
    | 115 ->
        "Expected statement.\n"
    | 79 ->
        "Expected \")\" after \"if\".\n"
    | 81 ->
        "Expected \")\".\n"
    | 82 ->
        "Expected statement.\n"
    | 107 ->
        "Expected statement.\n"
    | 113 ->
        "Expected statement after \"else\".\n"
    | 80 ->
        "Expected expression in if condition.\n"
    | 91 ->
        "Expected \";\" after halt statement.\n"
    | 93 ->
        "Expected \"(\" after assert keyword.\n"
    | 95 ->
        "Expected \")\" before \";\".\n"
    | 96 ->
        "Expected semicolon after assert statement.\n"
    | 94 ->
        "Expected expression after \"assert(\".\n"
    | _ ->
        raise Not_found
