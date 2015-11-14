open Ast;;
open Sast;;

(* translate flow ast to s_ast *)

type symbol_table = {
    parent : symbol_table option;
    variables : variable_decleration list;
}

type environment = {
    return_type : flow_type option;
    scope : symbol_table;
}

let rec find_variable (scope : symbol_table) (name : string) =
    try
        List.find (fun (var_decl) -> var_decl.declaration_id = name) scope.variables
    with Not_found ->
        match scope.parent with
            Some(parent) -> find_variable parent name
          | _ -> raise Not_found


