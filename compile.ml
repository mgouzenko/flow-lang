open Ast;;

let supported_channels = [Int;]

let compile (program : program) =
    let insert_boilerplate_header =
      "#include <stdio.h>\n"
    in
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
              with Not_found -> raise (Failure("Channel not supported")))
      | Struct(id) -> "struct " ^ id
      | Array(size, t) -> translate_type t
      | List(t) -> "wtf?" in

    let rec translate_expr (expr: expr) =
        let translate_bin_op exp1 bin_op exp2 = 
          match bin_op with
            Plus -> exp1 ^ "+" ^ exp2
          | Minus -> exp1 ^ "-" ^ exp2
          | Times -> exp1 ^ "*" ^ exp2
          | Divide -> exp1 ^ "/" ^ exp2
          | Modulo -> exp1 ^ "%" ^ exp2
          | Eq -> exp1 ^ "==" ^ exp2
          | Neq -> exp1 ^ "!=" ^ exp2
          | Lt -> exp1 ^ "<" ^ exp2
          | Gt -> exp1 ^ ">" ^ exp2
          | Leq -> exp1 ^ "<=" ^ exp2
          | Geq -> exp1 ^ ">=" ^ exp2
          | And -> exp1 ^ "&&" ^ exp2
          | Or -> exp1 ^ "||" ^ exp2
          | Send -> "enqueue_int(" ^ exp1 ^ ", " ^ exp2 ^ ")"
          | Assign -> exp1 ^ "=" ^ exp2
        in
        let translate_unary_op unary_op exp =
          match unary_op with
            Not -> "!" ^ exp
          | Negate -> "-" ^ exp
           (* TODO: In semantic analysis we need to check type to dequeue *)
          | Retrieve -> "dequeue_int(" ^ exp ^ ")"
          | Wait -> "wait_for_more(" ^ exp ^ ")"
        in
        let translate_bool b = 
          match b with
            true -> "1"
          | false -> "0"
        in
        (*let rec expr_list_to_string expr_list = 
          match expr_list with  
            [] -> ""
          | first::second::tail -> translate_expr first ^ "," ^ translate_expr second ^ "," ^ expr_list_to_string tail 
          | first::tail -> translate_expr first ^ expr_list_to_string tail 
        in *)
        let rec expr_list_to_string expr_list =
          List.fold_left (fun acc elm -> acc ^ ", " ^ (translate_expr elm))
             (translate_expr (List.hd expr_list)) (List.tl expr_list) 
        in
        match expr with
          IntLiteral(i) -> string_of_int i
        | StringLiteral(s) -> "\"" ^ s ^ "\""
        | BoolLiteral(b) -> translate_bool b
        | CharLiteral(c) -> "\'" ^ String.make 1 c ^ "\'"
        | DoubleLiteral(d) -> string_of_float d
        | Id(i) -> i
        | BinOp(expr1, bin_op, expr2) -> 
             translate_bin_op (translate_expr expr1) bin_op (translate_expr expr2)
        | UnaryOp(unary_op, expr) -> 
            translate_unary_op unary_op (translate_expr expr) 
        | Assign(id, expr) -> id ^ "=" ^ translate_expr expr
        | FunctionCall(id, expr_list) -> id ^ "(" ^ expr_list_to_string expr_list ^ ")"
        | StructInitializer(dot_initializer_list) -> "TODO"
        | Noexpr -> ""

     in

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

    let rec translate_stmt indentation_level (stmt: stmt) =
        let rec make_tabs num =
            if num = 0 then "" else ("\t" ^ make_tabs(num - 1)) in
        let cur_tabs = make_tabs indentation_level in

        cur_tabs ^
        (match stmt with
            Expr(e) -> translate_expr e ^ ";\n"
          | Block(stmt_list) ->
                  "{\n" ^
                  String.concat "" (List.map (translate_stmt (indentation_level+1)) stmt_list) ^
                  cur_tabs ^ "}\n"
          | Return(e) -> "return " ^ translate_expr e ^ ";"
          | Declaration(vdecl) -> translate_vdecl vdecl ^ ";\n"
          | If(e1, s1, s2) ->
                  "if(" ^ translate_expr e1 ^ ")\n" ^
                  translate_stmt (indentation_level + 1) s1 ^ "\n" ^
                  cur_tabs ^ "else" ^ "\n" ^
                  translate_stmt (indentation_level + 1) s2
          | For(e1, e2, e3, s) ->
                  "for(" ^ String.concat "; " (List.map translate_expr [e1; e2; e3]) ^
                  ")\n" ^ translate_stmt (indentation_level) s
          | While(e, s) ->
                  "while(" ^ translate_expr e ^ ")" ^
                  translate_stmt (indentation_level + 1) s
          | Continue -> "continue"
          | Break -> "break"
          | Poison(chan) -> "" (* TODO *)
        ) in

    (* unpacks the arguments to a process from void *_args *)
    let unpack_process_args (process: function_declaration) =
        "\n\t" ^
        (String.concat ";\n\t"
        (List.map (fun vdecl ->
            (translate_vdecl vdecl) ^ " = " ^
            "((struct _" ^ process.function_name ^ "_args*) _args)->" ^
            vdecl.declaration_id)
        process.arguments)) ^
        ";\n" in

    (* Translate flow function declaration to c function declaration *)
    let translate_fdecl (fdecl : function_declaration) =
        let arg_decl_string_list = (List.map translate_vdecl fdecl.arguments) in
        (match fdecl.return_type with
            Proc -> ("struct _" ^ fdecl.function_name ^ "_args{\n\t" ^
                     String.concat ";\n\t" arg_decl_string_list ^ ";\n};\n")
          | _ -> "") ^
        (translate_type fdecl.return_type) ^ " " ^
        fdecl.function_name ^
        (match fdecl.return_type with
            Proc -> "(void *_args)\n{" ^ (unpack_process_args fdecl)
          | _  -> "(" ^ (String.concat ", "  arg_decl_string_list) ^ ")\n{") ^
        String.concat "" (List.map (translate_stmt 1) fdecl.body) ^ "}"

    (* Tranlsate flow struct declaration to c struct declaration *)
    and translate_struct_decl (sdecl: struct_declaration) = "Sdecl" in

    (* Translate the flow program to a c program *)
    match program with Declarations(declaration_list) ->
        insert_boilerplate_header ^
        String.concat "\n"
        (List.map (fun decl ->
            match decl with
              VarDecl(vdecl) -> translate_vdecl vdecl
            | FuncDecl(fdecl) -> translate_fdecl fdecl
            | StructDecl(sdecl) -> translate_struct_decl sdecl)
        declaration_list) ^ "\n"
