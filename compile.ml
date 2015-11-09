open Ast;;

let supported_channels = [Int;]

let compile (program : program) =
    (* Translate flow type to c type *)
    let rec translate_type (ftype: flow_type) = match ftype with
        Int -> "int"
      | Float -> "float"
      | Double -> "double"
      | Bool -> "int"
      | Char -> "char"
      | Void -> "void"
      | Proc -> "void"
      | String -> "char *"
      | Channel(t, dir) ->
              (try
                  let _ = List.find (fun e -> t = e) supported_channels in
                  "struct " ^ translate_type t ^ "_channel* "
              with Not_found -> raise (Failure("channel not supported")))
      | Struct(id) -> id
      | Array(size, t) -> translate_type t
      | List(t) -> "wtf?" in

    (* Translate flow variable declaration to c variable declaration *)
    let translate_vdecl (vdecl : variable_declaration) = "VarDecl"

    (* Translate flow function declaration to c function declaration *)
    and translate_fdecl (fdecl : function_declaration) =
        (translate_type fdecl.return_type) ^ " " ^
        fdecl.function_name ^
        "\n{\nbody\n}"

    (* Tranlsate flow struct declaration to c struct declaration *)
    and translate_struct_decl (sdecl: struct_declaration) = "Sdecl" in

    (* Translate the flow program to a c program *)
    match program with Declarations(declaration_list) ->
        String.concat "\n"
        (List.map (fun decl ->
            match decl with
              VarDecl(vdecl) -> translate_vdecl vdecl
            | FuncDecl(fdecl) -> translate_fdecl fdecl
            | StructDecl(sdecl) -> translate_struct_decl sdecl)
        declaration_list)
