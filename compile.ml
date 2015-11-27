open Ast;;
open Sast;;
open Boilerplate;;

let supported_channels = [Int; Char;]

let compile (program : s_program) =

    (* Translate flow type to c type *)
    let rec translate_type (ftype: flow_type) = match ftype with
        Int -> "int"
      | Double -> "double"
      | Bool -> "int"
      | Char -> "char"
      | Void -> "void"
      | Proc -> "void*"
      | String -> "char *"
      | Channel(t, dir) ->
              (try
                  let _ = List.find (fun e -> t = e) supported_channels in
                  "struct _" ^ translate_type t ^ "_channel* "
              with Not_found -> raise (Failure("Channel not supported")))
      | Struct(id) -> "struct " ^ id
      | Array(t, size, id) ->  translate_type t ^ " " ^ id ^ "[" ^ string_of_int size ^ "]"
      | List(t) -> "wtf?" in

    (* Check that && and || for channels use _wait_for_more *)
    let check_wait_for_more exp t =
      match t with
        Channel(_,_) -> "_wait_for_more( (struct _channel*) " ^ exp ^ ")"
      | _ -> exp
    in

    let rec translate_expr (expr: typed_expr) =

        (* Ensure the correct type is enqueued on channel *)
        let enqueue_channel (typed_expr1: typed_expr) (typed_expr2 : typed_expr) =
          let t = snd typed_expr2 in
          match t with 
            Channel(Int, _) -> "_enqueue_int(" ^ (translate_expr typed_expr1) ^ ", " ^ (translate_expr typed_expr2) ^ ")"
          | Channel(Char, _) -> "_enqueue_char(" ^ (translate_expr typed_expr1) ^ ", " ^ (translate_expr typed_expr2) ^ ")"
          |  _ -> "" (* TODO Needs to be populated with other channel types *)
        in

        let translate_bin_op (typed_exp1 : typed_expr) (bin_op : bin_op) (typed_exp2 : typed_expr) = 
          let t1 = snd typed_exp1 
          and t2 = snd typed_exp2 
          and exp1 = translate_expr typed_exp1
          and exp2 = translate_expr typed_exp2
          in  
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
          | And -> (check_wait_for_more exp1 t1) ^ "&&" ^ (check_wait_for_more exp2 t2)
          | Or -> (check_wait_for_more exp1 t1) ^ "||" ^ (check_wait_for_more exp2 t2)
          | Send -> enqueue_channel typed_exp1 typed_exp2
          | Assign -> exp1 ^ "=" ^ exp2
        in

        (* Ensure the correct type is dequed from channel *)
        let dequeue_channel (typed_expr: typed_expr) =
          let t = snd typed_expr in
          match t with 
            Channel(Int, _) -> "_dequeue_int(" ^ translate_expr typed_expr ^ ")"
          | Channel(Char, _) -> "_dequeue_char(" ^ translate_expr typed_expr ^ ")" 
          |  _ -> "" (* TODO Needs to be populated with other channel types *)
        in

        let translate_unary_op (unary_op : unary_op) (typed_expr : typed_expr) =
          let exp = translate_expr typed_expr
          in 
          match unary_op with
            Not -> "!" ^ exp
          | Negate -> "-" ^ exp
          | Retrieve -> dequeue_channel typed_expr
        in
        let translate_bool b = 
          match b with
            true -> "1"
          | false -> "0"
        in
        let rec expr_list_to_string (expr_list : typed_expr list) =
          List.fold_left (fun acc elm -> acc ^ ", " ^ (translate_expr elm))
             (translate_expr (List.hd expr_list)) (List.tl expr_list) 
        in
        (* Translate flow type functions, including built-ins, to c function calls *)
        let translate_function (id : string) (expr_list : typed_expr list) : string =
            match id with
            | "print_string" -> "printf(\"%s\", " ^ expr_list_to_string expr_list ^ ")" 
            | "print_string_newline" -> "printf(\"%s\\n\", "^ expr_list_to_string expr_list ^ ")"
            | "print_int" -> "printf(\"%d\", " ^ expr_list_to_string expr_list ^ ")" 
            | "print_int_newline" -> "printf(\"%d\\n\", " ^ expr_list_to_string expr_list ^ ")" 
            | "print_char" -> "printf(\"%c\", " ^ expr_list_to_string expr_list ^ ")" 
            | "print_char_newline" -> "printf(\"%c\\n\", " ^ expr_list_to_string expr_list ^ ")" 
            | _ -> id ^ "(" ^ expr_list_to_string expr_list ^ ")"
        in
        let translate_process_call (id : string) (expr_list : typed_expr list) =
          let pthread_decl = "pthread_t* _t = _make_pthread_t();\n" in
          let args_struct = "struct _" ^ id ^ "_args _args = {\n" ^ expr_list_to_string expr_list ^ "\n};\n" in
          let pthread_creation = "pthread_create(_t, NULL, " ^ id ^ ", (void *) &_args);\n" in 
          "{\n" ^ pthread_decl ^ args_struct ^ pthread_creation ^ "\n}"
        in
        match expr with
          TIntLiteral(i), _ -> string_of_int i
        | TStringLiteral(s), _ -> "\"" ^ s ^ "\""
        | TBoolLiteral(b), _ -> translate_bool b
        | TCharLiteral(c), _ -> "\'" ^ String.make 1 c ^ "\'"
        | TDoubleLiteral(d), _ -> string_of_float d
        | TId(i), _ -> i
        | TBinOp(expr1, bin_op, expr2), _ -> 
             translate_bin_op expr1 bin_op expr2
        | TUnaryOp(unary_op, expr), _ -> 
            translate_unary_op unary_op expr 
        | TFunctionCall(id, expr_list), Proc -> translate_process_call id expr_list  
        | TFunctionCall(id, expr_list), _ -> translate_function id expr_list  
        | TStructInitializer(dot_initializer_list), _ -> "TODO"
        | TArrayInitializer(expr_list), _ -> "{" ^ expr_list_to_string expr_list ^ "}"
        | TArrayElement(id, expr), _ -> id ^ "[" ^ translate_expr expr ^ "]"
        | TNoexpr, _ -> ""

    in

    (* Get the type of the channel *)
    let get_channel_type (chan: flow_type) = 
      match chan with 
        Channel(Int, _) -> "_int_channel"
      | Channel(Char, _) -> "_char_channel"
      | _ -> ""
    in

    (* Translate flow variable declaration to c variable declaration *)
    let translate_vdecl (vdecl : s_variable_declaration) =
        let translated_type = translate_type vdecl.s_declaration_type in
        translated_type ^ " " ^
        vdecl.s_declaration_id ^
        (match vdecl.s_declaration_type with
            (* If the declaration is a channel, we need to perform a malloc
             * and also initialize the struct associated with the channel *)
            Channel(t, Nodir) ->
                let channel_type = get_channel_type vdecl.s_declaration_type in

                 (* Casts malloc'ed pointer *)
                ("= (" ^ translated_type ^ ")" ^

                 (* Perform the malloc with the proper struct *)
                 "malloc(sizeof(" ^ "struct " ^ channel_type ^ "));\n" ^

                  (* This will initializes the locks, etc. *)
                 "_init_channel( (struct _channel *) " ^ vdecl.s_declaration_id ^ ")")

            (* If it's not a channel, the variable may or may not need to be
             * initialized. *)
          | _ ->
                (match vdecl.s_declaration_initializer with
                    TNoexpr, _ -> ""
                  | _, _ -> "=" ^ (translate_expr vdecl.s_declaration_initializer))) in

    (* Check if channel is in a conditional *)
    let eval_conditional_expr (typed_expr :typed_expr) =
      let t = snd typed_expr in
      match t with
        Channel(_,_) -> "_wait_for_more((struct _channel* ) " ^ translate_expr typed_expr ^ ")"
      | _ ->  translate_expr typed_expr (* TODO Needs to be populated with other channel types *)
    in

    (* Check the type of the poison token *)
    let eval_poison_type (typed_expr : typed_expr) =
      let t = snd typed_expr in
      match t with
        Channel(_,_) -> "_poison((struct _channel* )" ^ translate_expr typed_expr ^ ");"
      | _ -> translate_expr typed_expr (* TODO Needs to be populated with other channel types *)
    in

    let rec translate_stmt (stmt: s_stmt) =
        match stmt with
            SExpr(e) -> translate_expr e ^ ";\n"
          | SBlock(stmt_list) ->
                  "{\n" ^
                  String.concat "" (List.map translate_stmt stmt_list) ^
                  "}\n"
          | SReturn(e) -> "return " ^ translate_expr e ^ ";"
          | SDeclaration(vdecl) -> translate_vdecl vdecl ^ ";\n"
          | SIf(e1, s1, s2) ->
                  "if(" ^ eval_conditional_expr e1 ^ ")\n" ^
                  translate_stmt s1 ^ "\nelse\n" ^
                  translate_stmt s2
          | SFor(e1, e2, e3, s) ->
                  "for(" ^ String.concat "; " (List.map translate_expr [e1; e2; e3]) ^
                  ")\n" ^ translate_stmt s
          | SWhile(e, s) ->
                  "while(" ^ eval_conditional_expr e ^ ")" ^
                  translate_stmt s
          | SContinue -> "continue;"
          | SBreak -> "break;"
          | SPoison(e) -> eval_poison_type e in

    (* unpacks the arguments to a process from void *_args *)
    let unpack_process_args (process: s_function_declaration) =
        "\n" ^
        (String.concat ";\n"
        (List.map (fun vdecl ->
            (translate_vdecl vdecl) ^ " = " ^
            "((struct _" ^ process.s_function_name ^ "_args*) _args)->" ^
            vdecl.s_declaration_id)
        process.s_arguments)) ^
        ";\n" in

    (* Translate flow function declaration to c function declaration *)
    let translate_fdecl (fdecl : s_function_declaration) : string =
        let opening_stmts, closing_stmts =
            if fdecl.s_function_name = "main"
            then "pthread_mutex_init(&_thread_list_lock, NULL);\n", "_wait_for_finish();\n"
            else "","" in
        let arg_decl_string_list = (List.map translate_vdecl fdecl.s_arguments) in
        (match fdecl.s_return_type with
            Proc -> ("struct _" ^ fdecl.s_function_name ^ "_args{\n\t" ^
                     String.concat ";\n\t" (List.rev arg_decl_string_list) ^ ";\n};\n")
          | _ -> "") ^
        (translate_type fdecl.s_return_type) ^ " " ^
        fdecl.s_function_name ^
        (match fdecl.s_return_type with
            Proc -> "(void *_args)\n{" ^ (unpack_process_args fdecl)
          | _  -> "(" ^ (String.concat ", "  arg_decl_string_list) ^ ")\n{") ^ opening_stmts ^
        String.concat "" (List.map translate_stmt fdecl.s_body) ^ closing_stmts ^ "\n}"

    (* Tranlsate flow struct declaration to c struct declaration *)
    and translate_struct_decl (sdecl: s_struct_declaration) = "Sdecl" in

    (* Translate the flow program to a c program *)
    boilerplate_header ^
    String.concat "\n"
    (List.map (fun decl ->
        match decl with
          SVarDecl(vdecl) -> translate_vdecl vdecl
        | SFuncDecl(fdecl) -> translate_fdecl fdecl
        | SStructDecl(sdecl) -> translate_struct_decl sdecl)
    (List.rev program)) ^ "\n"
