open Ast;;

let supported_channels = [Int;]

let compile (program : program) =
    let insert_boilerplate_header =
"
 #include <assert.h>
 #include <pthread.h>
 #include <stdio.h>
 #include <stdlib.h>
 #include <stdbool.h>

struct _int_channel{
  int queue[100];
  int front;
  int back;     // One past the last element
  int MAX_SIZE;
  int size;
  bool poisoned;
  pthread_mutex_t lock;
  pthread_cond_t write_ready;
  pthread_cond_t read_ready;
};

int _init_int_channel(struct _int_channel *channel){
  if(pthread_mutex_init(&channel->lock, NULL) != 0){
      printf(\"Mutex init failed\");
      return 1;
  }

  if(pthread_cond_init(&channel->write_ready, NULL) +
          pthread_cond_init(&channel->read_ready, NULL ) != 0){
      printf(\"Cond init failed\");
      return 1;
  }
  channel->MAX_SIZE = 100;
  channel->front = 0;
  channel->back = 0;
  channel->poisoned = false;
  return 0;
}

void _enqueue_int(int element, struct _int_channel *channel){
    pthread_mutex_lock(&channel->lock);
    while(channel->size >= channel->MAX_SIZE)
        pthread_cond_wait(&channel->write_ready, &channel->lock);

    assert(channel->size < channel->MAX_SIZE);
    assert(!(channel->poisoned));

    channel->queue[channel->back] = element;
    channel->back = (channel->back + 1) % channel->MAX_SIZE;

    channel->size++;
    pthread_cond_signal(&channel->read_ready);
    pthread_mutex_unlock(&channel->lock);
}

int _dequeue_int(struct _int_channel *channel){
    pthread_mutex_lock(&channel->lock);
    assert(channel->size != 0);

    int result = channel->queue[channel->front];
    channel->front = (channel->front + 1) % channel->MAX_SIZE;

    channel->size--;
    pthread_cond_signal(&channel->write_ready);
    pthread_mutex_unlock(&channel->lock);
    return result;
}

void _poison(struct _int_channel *channel) {
    pthread_mutex_lock(&channel->lock);
    channel->poisoned = true;
    pthread_cond_signal(&channel->read_ready);
    pthread_mutex_unlock(&channel->lock);
}

bool _wait_for_more(struct _int_channel *channel) {
    pthread_mutex_lock(&channel->lock);
    while(channel->size == 0) {
        if(channel->poisoned){
            pthread_mutex_unlock(&channel->lock);
            return false;
        }
        else {
            pthread_cond_wait(&channel->read_ready, &channel->lock);
        }
    }
    pthread_mutex_unlock(&channel->lock);
    return true;

}

struct _pthread_node{
  pthread_t thread;
  struct _pthread_node *next;
};

struct _pthread_node* _head = NULL;

pthread_mutex_t _thread_list_lock;

pthread_t* _make_pthread_t() {
  pthread_mutex_lock(&_thread_list_lock);
  struct _pthread_node *new_pthread = (struct _pthread_node *) malloc(sizeof(struct _pthread_node));
  new_pthread->next = _head;
  _head = new_pthread;
  pthread_mutex_unlock(&_thread_list_lock);
  return &(new_pthread->thread);
}

void _wait_for_finish(){
  struct _pthread_node* curr = _head;
  while(curr){
    pthread_join(curr->thread, NULL);
    curr = curr->next;
  }

}\n"
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
                  "struct _" ^ translate_type t ^ "_channel* "
              with Not_found -> raise (Failure("Channel not supported")))
      | Struct(id) -> "struct " ^ id
      | Array(size, t) ->  translate_type t ^ "[" ^ string_of_int size ^ "]"
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
          | Send -> "_enqueue_int(" ^ exp1 ^ ", " ^ exp2 ^ ")"
          | Assign -> exp1 ^ "=" ^ exp2
        in
        let translate_unary_op unary_op exp =
          match unary_op with
            Not -> "!" ^ exp
          | Negate -> "-" ^ exp
           (* TODO: In semantic analysis we need to check type to dequeue *)
          | Retrieve -> "_dequeue_int(" ^ exp ^ ")"
          | Wait -> "_wait_for_more(" ^ exp ^ ")"
        in
        let translate_bool b = 
          match b with
            true -> "1"
          | false -> "0"
        in
        let rec expr_list_to_string expr_list =
          List.fold_left (fun acc elm -> acc ^ ", " ^ (translate_expr elm))
             (translate_expr (List.hd expr_list)) (List.tl expr_list) 
        in
        let translate_process_call id expr_list =
          let pthread_decl = "pthread_t* _t = _make_pthread_t();\n" in
          let args_struct = "struct _" ^ id ^ "_args _args = {\n" ^ expr_list_to_string expr_list ^ "\n};\n" in
          let pthread_creation = "pthread_create(_t, NULL, " ^ id ^ ", (void *) &_args);\n" in 
          "{\n" ^ pthread_decl ^ args_struct ^ pthread_creation ^ "\n}"
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
        | ProcessCall(id, expr_list) -> translate_process_call id expr_list
        | StructInitializer(dot_initializer_list) -> "TODO"
        | ArrayInitializer(expr_list) -> "{" ^ expr_list_to_string expr_list ^ "}"
        | ArrayElement(id, expr) -> id ^ "[" ^ translate_expr expr ^ "]"
        | Noexpr -> ""

    in

    (* Translate flow variable declaration to c variable declaration *)
    let translate_vdecl (vdecl : variable_declaration) =
        let translated_type = translate_type vdecl.declaration_type in
        translated_type ^ " " ^
        vdecl.declaration_id ^
        (match vdecl.declaration_type with
            Channel(t, Nodir) -> ("= (" ^ translated_type ^
                                  ") malloc(sizeof(" ^ "struct _int_channel" ^ (*^ translated_type ^ *)(*this doesn't work*) "));\n" ^ (* AWFUL code *)
                                  "_init_int_channel(" ^ vdecl.declaration_id ^ ")") (* AWFUL code TODO: must fix *)
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
          | Continue -> "continue;"
          | Break -> "break;"
          | Poison(chan) -> "_poison(" ^ translate_expr chan ^ ");"
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
        let opening_stmts = if fdecl.function_name = "main" then "pthread_mutex_init(&_thread_list_lock, NULL);\n" else "" in
        let closing_stmts = if fdecl.function_name = "main" then "_wait_for_finish();\n" else "" in
        let arg_decl_string_list = (List.map translate_vdecl fdecl.arguments) in
        (match fdecl.return_type with
            Proc -> ("struct _" ^ fdecl.function_name ^ "_args{\n\t" ^
                     String.concat ";\n\t" (List.rev arg_decl_string_list) ^ ";\n};\n")
          | _ -> "") ^
        (translate_type fdecl.return_type) ^ " " ^
        fdecl.function_name ^
        (match fdecl.return_type with
            Proc -> "(void *_args)\n{" ^ (unpack_process_args fdecl)
          | _  -> "(" ^ (String.concat ", "  arg_decl_string_list) ^ ")\n{") ^ opening_stmts ^
        String.concat "" (List.map (translate_stmt 1) fdecl.body) ^ closing_stmts ^ "\nreturn 0;}" (* WTF: TODO: return 0 should NOT be at the end of every function*)

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
