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
      | Proc -> "void*"
      | String -> "char *"
      | Channel(t, dir) ->
              (try
                  let _ = List.find (fun e -> t = e) supported_channels in
                  "struct " ^ translate_type t ^ "_channel* "
              with Not_found -> raise (Failure("channel not supported")))
      | Struct(id) -> id
      | Array(size, t) -> translate_type t
      | List(t) -> "wtf?" in

    let translate_expr (expr: expr) = "Expr" in

    (* Translate flow variable declaration to c variable declaration *)
    let translate_vdecl (vdecl : variable_declaration) =
        let translated_type = translate_type vdecl.declaration_type in
        translated_type ^ " " ^
        vdecl.declaration_id ^
        (match vdecl.declaration_type with
            Channel(t, Nodir) -> ("(" ^ translated_type ^
                                  ") = malloc(sizeof(" ^ translated_type)
          | _ -> (match vdecl.declaration_initializer with
                      Noexpr -> ""
                    | _ -> "=" ^ (translate_expr vdecl.declaration_initializer))) in

    (* unpacks the arguments to a process from void *_args *)
    let unpack_process_args (process: function_declaration) =
        String.concat "\n"
        (List.map (fun vdecl ->
            (translate_vdecl vdecl) ^ " = " ^
            "((struct _" ^ process.function_name ^ "_args*) _args)->" ^
            vdecl.declaration_id)
        process.arguments) in

    (* Translate flow function declaration to c function declaration *)
    let translate_fdecl (fdecl : function_declaration) =
        (translate_type fdecl.return_type) ^ " " ^
        fdecl.function_name ^
        (match fdecl.return_type with
            Proc -> "(void *_args)\n{\n" ^ (unpack_process_args fdecl)
          | _  -> "(" ^ String.concat ", " (List.map translate_vdecl fdecl.arguments) ^ ")\n{") ^
        "\nbody\n}"

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
