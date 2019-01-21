
(* This file was auto-generated based on "frontend/parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "Expected statement.\n"
    | 92 ->
        "Expected \";\".\n"
    | 28 ->
        "Expected \";\".\n"
    | 29 ->
        "Expected expression.\n"
    | 94 ->
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
    | 93 ->
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
    | 59 ->
        "Expected \"(\" after print statement.\n"
    | 60 ->
        "Expected lvalue in print statement.\n"
    | 62 ->
        "Expected \",\" between each lvalue in print statement.\n"
    | 63 ->
        "Expected lvalue in print statement.\n"
    | 66 ->
        "Expected \";\" after print statement.\n"
    | 68 ->
        "Expected statement.\n"
    | 108 ->
        "Expected statement.\n"
    | 104 ->
        "Expected statement.\n"
    | 69 ->
        "Expected \")\" after \"if\".\n"
    | 71 ->
        "Expected \")\".\n"
    | 72 ->
        "Expected statement.\n"
    | 96 ->
        "Expected statement.\n"
    | 102 ->
        "Expected statement after \"else\".\n"
    | 70 ->
        "Expected expression in if condition.\n"
    | 81 ->
        "Expected \";\" after halt statement.\n"
    | 83 ->
        "Expected \"(\" after assert keyword.\n"
    | 85 ->
        "Expected \")\" before \";\".\n"
    | 86 ->
        "Expected semicolon after assert statement.\n"
    | 84 ->
        "Expected expression after \"assert(\".\n"
    | _ ->
        raise Not_found
