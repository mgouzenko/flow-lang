open Ast
open Sast
open Boilerplate

let supported_channels = [ Int; Char; Double ]
let supported_lists = [ Int; Char; Double; Channel (Int, Nodir) ]

let compile (program : s_program) (dot : bool) : string =

  (* Toggle whether the resulting c program should print a dot graph *)
  let print_dot = if dot then "true" else "false" in

  (* Translate flow type to c type *)
  let rec translate_type (ftype : flow_type) =
    match ftype with
    | Int -> "int"
    | Double -> "double"
    | Bool -> "int"
    | Char -> "char"
    | Void -> "void"
    | Proc -> "void*"
    | String -> "char *"
    | Channel (t, dir) ->
        (try
           let _ = List.find (fun e -> t = e) supported_channels
           in "struct _" ^ ((translate_type t) ^ "_channel* ")
         with | Not_found -> raise (Failure "Channel not supported"))
    | List t ->
        (try
           let _ = List.find (fun e -> t = e) supported_lists
           in "struct _cell *"
         with | Not_found -> raise (Failure "List not supported")) in

  (* Wrap channels used in a logical context in _wait_for_more *)
  let wait_for_more (exp: string) (t: flow_type) : string =
    match t with
    | Channel (_, _) -> "_wait_for_more( (struct _channel*) " ^ (exp ^ ")")
    | _ -> exp in

  (* Translate a flow expression *)
  let rec translate_expr (expr : typed_expr) : string =
    let translate_bin_op (typed_exp1 : typed_expr) (bin_op : bin_op)
                         (typed_exp2 : typed_expr) =
      let t1 = snd typed_exp1
      and t2 = snd typed_exp2
      and exp1 = translate_expr typed_exp1
      and exp2 = translate_expr typed_exp2
      in
        match bin_op with
        | Plus -> exp1 ^ ("+" ^ exp2)
        | Minus -> exp1 ^ ("-" ^ exp2)
        | Times -> exp1 ^ ("*" ^ exp2)
        | Divide -> exp1 ^ ("/" ^ exp2)
        | Modulo -> exp1 ^ ("%" ^ exp2)
        | Eq -> exp1 ^ ("==" ^ exp2)
        | Neq -> exp1 ^ ("!=" ^ exp2)
        | Lt -> exp1 ^ ("<" ^ exp2)
        | Gt -> exp1 ^ (">" ^ exp2)
        | Leq -> exp1 ^ ("<=" ^ exp2)
        | Geq -> exp1 ^ (">=" ^ exp2)
        | And ->
            (wait_for_more exp1 t1) ^
              ("&&" ^ (wait_for_more exp2 t2))
        | Or ->
            (wait_for_more exp1 t1) ^
              ("||" ^ (wait_for_more exp2 t2))
        | Send ->
            "CALL_ENQUEUE_FUNC(" ^
              (exp1 ^
                 (", " ^
                    (exp2 ^
                       ("," ^
                          ((translate_type t1) ^ (", " ^ (print_dot ^ ")")))))))
        | Assign ->
            (match (t1, (fst typed_exp2)) with
             | (List t, TListInitializer _) ->
                 let temp_list_name = "_temp_" ^ exp1 in
                 let temp_vdecl =
                   {
                     s_declaration_type = List t;
                     s_declaration_id = temp_list_name;
                     s_declaration_initializer = typed_exp2;
                   }
                 in
                   (translate_vdecl temp_vdecl false) ^
                     (";\n" ^ (exp1 ^ ("=" ^ (temp_list_name ^ ";\n"))))
             | _ -> exp1 ^ ("=" ^ exp2))
        | Concat ->
             "_add_front( (union _payload) " ^ ( "(" ^ exp1 ^ ")" ^ (", " ^ (exp2 ^ ")"))) in

    (* Translate a flow unary operation *)
    let translate_unary_op (unary_op : unary_op) (typed_expr : typed_expr) : string =
      let exp = translate_expr typed_expr
      in
        match unary_op with
        | Not -> "!" ^ exp
        | Negate -> "-" ^ exp
        | Retrieve ->
            (match snd typed_expr with
                 Channel (t, dir) ->
                 "CALL_DEQUEUE_FUNC(" ^
                   (exp ^
                      ("," ^
                         ((translate_type t) ^ (", " ^ (print_dot ^ ")")))))
             | List t ->
                 let type_to_union_element =
                   (function
                    | Int -> "_int"
                    | Double -> "_double"
                    | Char -> "_char"
                    | Channel (Int, _) -> "_int_channel"
                    | _ -> "")
                 in
                   "_get_front(" ^ (exp ^ (")." ^ (type_to_union_element t)))
             | _ -> raise (Failure "Invalid type"))
        | ListLength -> "_get_length(" ^ (exp ^ ")")
        | ListTail -> "_get_tail(" ^ (exp ^ ")") in

    let translate_bool b = match b with | true -> "1" | false -> "0" in

    (* Translate a comma separated list of expressions *)
    let rec translate_expr_list (expr_list : typed_expr list) : string =
      let translated_exprs =
        List.rev
          (List.fold_left (fun acc elm -> (translate_expr elm) :: acc) []
             expr_list)
      in String.concat ", " translated_exprs in

    (* Translate flow type functions, including built-ins, to c function calls *)
    let translate_function_call (id : string) (expr_list : typed_expr list) :
      string =
      match id with
      | "print_string" ->
          "printf(\"%s\", " ^
            ((translate_expr_list expr_list) ^ (");\n" ^ "fflush(stdout)"))
      | "print_int" ->
          "printf(\"%d\", " ^
            ((translate_expr_list expr_list) ^ (");\n" ^ "fflush(stdout)"))
      | "print_char" ->
          "printf(\"%c\", " ^
            ((translate_expr_list expr_list) ^ (");\n" ^ "fflush(stdout)"))
      | "print_double" ->
          "printf(\"%G\", " ^
            ((translate_expr_list expr_list) ^ (");\n" ^ "fflush(stdout)"))
      | "println" -> "printf(\"\\n\");\n" ^ "fflush(stdout)"
      | "rand" -> " (double)rand() / (double)RAND_MAX "
      | _ -> id ^ ("(" ^ ((translate_expr_list expr_list) ^ ")")) in

    (* Translate flow process invocations to c pthread_create's *)
    let translate_process_call (id : string) (expr_list : typed_expr list) : string =
      let pthread_decl =
        "pthread_t* _t = _make_pthread_t(\"" ^ (id ^ "\");\n") in
      let malloced_args =
        "struct _" ^
          (id ^
             ("_args* _margs = malloc(sizeof(struct _" ^ (id ^ "_args));\n"))) in

      (* Collect the args into a struct on the stack *)
      let args_struct =
        "struct _" ^
          (id ^
             ("_args _args = {\n" ^
                ((translate_expr_list expr_list) ^ "\n};\n"))) in

      (* Copy the struct over to the heap *)
      let copy_struct =
        "memcpy((void*) _margs, (void*) &_args, sizeof(typeof(_args)));\n" in

      (* Create the pthread for this process *)
      let pthread_creation =
        "pthread_create(_t, NULL, " ^ (id ^ ", (void *) _margs);\n")
      in
        "{\n" ^
          (pthread_decl ^
             (malloced_args ^
                (args_struct ^ (copy_struct ^ (pthread_creation ^ "\n}")))))
    in
      match expr with
      | (TIntLiteral i, _) -> string_of_int i
      | (TStringLiteral s, _) -> "\"" ^ (s ^ "\"")
      | (TBoolLiteral b, _) -> translate_bool b
      | (TCharLiteral c, _) -> "\'" ^ ((String.make 1 c) ^ "\'")
      | (TDoubleLiteral d, _) -> string_of_float d
      | (TId i, _) -> i
      | (TBinOp (expr1, bin_op, expr2), _) ->
          translate_bin_op expr1 bin_op expr2
      | (TUnaryOp (unary_op, expr), _) -> translate_unary_op unary_op expr
      | (TFunctionCall (id, expr_list), Proc) ->
          translate_process_call id expr_list
      | (TFunctionCall (id, expr_list), _) -> translate_function_call id expr_list
      | (TListInitializer expr_list, _) ->
          "{" ^ ((translate_expr_list expr_list) ^ "}")
      | (TNoexpr, _) -> ""

  (* Translate flow variable declaration to c variable declaration *)
  and translate_vdecl (vdecl : s_variable_declaration) (is_arg : bool) =
    let translated_type = translate_type vdecl.s_declaration_type in
    translated_type ^
        (" " ^
           (vdecl.s_declaration_id ^
              (" " ^
                 (* This portion deals with initializing variables. *)
                 (match vdecl.s_declaration_type with
                    (* If the declaration is a channel, we need to perform a malloc
                     * and also initialize the struct associated with the channel *)
                  | Channel (t, Nodir) ->
                      (* If channel being translated arg, no malloc needed *)
                      if is_arg then ""
                      else
                        (match fst vdecl.s_declaration_initializer with
                         | TNoexpr ->
                             (* Perform the malloc with the proper struct.
                              * This is taken care of by the runtime with the
                              * MALLOC_CHANNEL macro *)
                             "MALLOC_CHANNEL(" ^
                               ((translate_type t) ^
                                  (");\n" ^
                                     (* This will initializes the locks, flags, etc. *)
                                     ("_init_channel( (struct _channel *) " ^
                                        (vdecl.s_declaration_id ^ ")"))))

                           (* Scenario where a channel is dequeued from a list
                            * or returned from a function *)
                         | TUnaryOp (Retrieve, _) | TFunctionCall (_, _) ->
                             " = " ^
                               (translate_expr
                                  vdecl.s_declaration_initializer)
                         | _ -> "")

                  | List t ->
                      (* If the list is an arg, it need not be initialized *)
                      if is_arg then ""
                      else
                        (let list_initialization_statements =
                           (* Lists can be initialized in a number of ways, ranging
                            * from intialization lists to function calls, to assignment. *)
                           match fst vdecl.s_declaration_initializer with
                           | TListInitializer expr_list ->
                               List.map
                                 (* Call _add_front on every expression in the initializer *)
                                 (fun expr ->
                                    vdecl.s_declaration_id ^
                                      (" = _add_front( (union _payload)" ^
                                         ((translate_expr expr) ^
                                            ("," ^
                                               (vdecl.s_declaration_id ^ ")")))))
                                 (List.rev expr_list)
                           | TUnaryOp (ListTail, _) ->
                               [ vdecl.s_declaration_id ^
                                   ("=" ^
                                      (translate_expr
                                         vdecl.s_declaration_initializer)) ]
                           | TId id_name ->
                               [ vdecl.s_declaration_id ^ ("=" ^ id_name);
                                 "_increase_refs(" ^ (id_name ^ ")") ]
                           | TFunctionCall (_, _) ->
                               [ vdecl.s_declaration_id ^
                                   ("=" ^
                                      (translate_expr
                                         vdecl.s_declaration_initializer)) ]
                           | TNoexpr -> [ "" ]
                           | _ -> raise (Failure "Invalid list initializer ")
                         in "= NULL; " ^ (String.concat ";\n" list_initialization_statements))
                  | _ ->
                      (match vdecl.s_declaration_initializer with
                       | (TNoexpr, _) -> ""
                       | (_, _) ->
                           "=" ^
                             (translate_expr vdecl.s_declaration_initializer)))))) in

  (* Translates specifically those expressions that are used in a
   * boolean context. This is necessary to check if channel used as a
   * boolean. *)
  let translate_boolean_expr (typed_expr : typed_expr) : string =
    let t = snd typed_expr
    in
      match t with
      | Channel (_, _) ->
          "_wait_for_more((struct _channel* ) " ^
            ((translate_expr typed_expr) ^ ")")
      | _ -> translate_expr typed_expr in

  (* Check the type of the poison token *)
  let translate_poison_expr (typed_expr : typed_expr) : string =
    let t = snd typed_expr
    in
      match t with
      | Channel (_, _) ->
          "_poison((struct _channel* )" ^
            ((translate_expr typed_expr) ^ ");")
      | _ -> translate_expr typed_expr in

  (* Translate a flow statement to a C statement *)
  let rec translate_stmt (stmt : s_stmt) : string =
    match stmt with
    | SExpr e ->
        let translated_expr = (translate_expr e) ^ ";\n"
        in
          (match fst e with
           | (* This absurd match finds all list assignments for ref counting *)
               TBinOp (e1, op, e2) when (op = Assign) &&
                (match snd e1 with | List _ -> true | _ -> false) ->
                    let list_name = translate_expr e1 in
                    let store_temp = "temp = " ^ (list_name ^ ";\n") in
                    let dec_stmt = "_decrease_refs(temp);\n"
                    and inc_stmt = "_increase_refs(" ^ (list_name ^ ");\n") in
                    (* First decrease the references to the list, in
                     * prep for reassignment. Then, do the reassignment.
                     * Then, increase the references to the list, which
                     * now points to a new cell after reassignment *)
                 store_temp ^ (translated_expr ^ (inc_stmt ^ dec_stmt))
           | _ -> translated_expr)
    | SBlock stmt_list ->
        "{\n" ^
          ((String.concat "" (List.map translate_stmt stmt_list)) ^ "}\n")
    | SReturn e -> "return " ^ ((translate_expr e) ^ ";")
    | SDeclaration vdecl -> (translate_vdecl vdecl false) ^ ";\n"
    | SIf (e1, s1, s2) ->
        "if(" ^
          ((translate_boolean_expr e1) ^
             (")\n" ^
                ((translate_stmt s1) ^ ("\nelse\n" ^ (translate_stmt s2)))))
    | SFor (e1, e2, e3, s) ->
        "for(" ^
          ((String.concat "; " (List.map translate_expr [ e1; e2; e3 ])) ^
             (")\n" ^ (translate_stmt s)))
    | SWhile (e, s) ->
        "while(" ^ ((translate_boolean_expr e) ^ (")" ^ (translate_stmt s)))
    | SContinue -> "continue;"
    | SBreak -> "break;"
    | SPoison e -> translate_poison_expr e
    | SExitProc -> "_exit_thread();" in

  (* unpacks the arguments to a process from void *_args *)
  let unpack_process_args (process : s_function_declaration) : string =
    "\n" ^
      ((String.concat ";\n"
          (List.map
             (fun vdecl ->
                (translate_vdecl vdecl true) ^
                  (" = " ^
                     ("((struct _" ^
                        (process.s_function_name ^
                           ("_args*) _args)->" ^ vdecl.s_declaration_id)))))
             process.s_arguments))
         ^ ";\n")
  in

  (* Translate flow function declaration to c function declaration *)
  let translate_fdecl (fdecl : s_function_declaration) : string =

    (* Opening and closing statments are required to in main to initialize
     * and clean up the environment, respectively *)
    let (opening_stmts, closing_stmts) =
      if fdecl.s_function_name = "main"
      then
        (("_initialize_runtime(" ^ (print_dot ^ ");\n")),
         ("_wait_for_finish(" ^ (print_dot ^ ");\n")))
      else ("", "")

    (* temp is a helper variable present in every function. It's used to
     * juggle around temporary lists during list reassignment for the
     * purpose of reference counting. *)
    and temp_list_decl = "struct _cell* temp;\n" in

    (* Translate the function's argument declarations *)
    let arg_decl_string_list =
      List.map (fun arg -> translate_vdecl arg true) fdecl.s_arguments
    in
      (* Procs must have their args bundled into a struct. So, during the
       * declaration of a proc, that proc's argument struct must be declared. *)
      (match fdecl.s_return_type with
       | Proc ->
           "struct _" ^
             (fdecl.s_function_name ^
                ("_args{\n\t" ^
                   ((String.concat ";\n\t" arg_decl_string_list) ^ ";\n};\n")))
       | _ -> "") ^
        (* This is the actual c function declaration *)
        ((translate_type fdecl.s_return_type) ^
           (" " ^
              (fdecl.s_function_name ^
                 ((match fdecl.s_return_type with
                   | Proc -> "(void *_args)\n{" ^ (unpack_process_args fdecl)
                   | _ ->
                       "(" ^
                         ((String.concat ", " arg_decl_string_list) ^ ")\n{")) ^
                    (opening_stmts ^
                       (temp_list_decl ^
                          ((String.concat ""
                              (List.map translate_stmt fdecl.s_body))
                             ^ (closing_stmts ^ "\n}"))))))))
  in

    (* Translate the flow program to a c program *)
    boilerplate_header ^      (* The base c runtime environment *)
      ((String.concat "\n"    (* This translates all top-level function and variable decls. *)
          (List.map
             (fun decl ->
                match decl with
                | SVarDecl vdecl -> (translate_vdecl vdecl false) ^ ";\n"
                | SFuncDecl fdecl -> translate_fdecl fdecl)
             (List.rev program)))
         ^ "\n")
