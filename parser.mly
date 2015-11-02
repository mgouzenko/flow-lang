%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA
%token SHIFT_RIGHT SHIFT_LEFT BIT_OR BIT_NOT BIT_AND BIT_XOR
%token PLUS MINUS TIMES DIVIDE MODULO ASSIGN
%token WRITE_CHANNEL READ_CHANNEL PROC CHANNEL IN OUT
%token DOT
%token BREAK CONTINUE VOID
%token OR AND NOT
%token DOUBLE CHAR BOOL INT STRING LIST STRUCT
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE
%token <int> INT_LITERAL
%token <float> DOUBLE_LITERAL
%token <char> CHAR_LITERAL
%token <bool> BOOL_LITERAL
%token <string> STRING_LITERAL
%token <string> IDENTIFIER
%token EOF

%nonassoc NOELSE /* dummy variable for lowest precedence */
%nonassoc ELSE
%right ASSIGN
%left AND OR
%right BIT_AND BIT_OR BIT_XOR
%left EQ NEQ
%left LT GT LEQ GEQ
%right SHIFT_LEFT SHIFT_RIGHT
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left WRITE_CHANNEL READ_CHANNEL
%left DOT
%nonassoc UNARY_OP  /* dummy variable for highest precedence */

%start program
%type <Ast.program> program

%%

program:
  declarations EOF {Declarations($1)}

declarations:
    /* nothing */ { [] }
  | declarations var_declaration SEMI { VarDecl($2)::$1  }
  | declarations function_declaration { FuncDecl($2)::$1 }
  | declarations struct_declaration   { StructDecl($2)::$1 }

function_declaration:
    flow_type IDENTIFIER LPAREN arg_decl_list RPAREN LBRACE stmt_list RBRACE
    {
        {
            return_type = $1;
            process_name = $2;
            arguments = $4;
            body = List.rev $7;
        }
    }

arg_decl_list:
    /* nothing */ {[]}
  | arg_decl {[$1]}
  | arg_decl_list COMMA arg_decl {$3::$1}

arg_decl:
    simple_var_declaration {match $1.declaration_type with
                              Channel(t, dir) when dir = Nodir ->
                                  raise (Failure "Channel argument needs direction")
                            | _ -> $1}

flow_type:
    INT                       {Int}
  | DOUBLE                    {Double}
  | CHAR                      {Char}
  | BOOL                      {Bool}
  | VOID                      {Void}
  | PROC                      {Proc}
  | STRING                    {String}
  | IDENTIFIER                {Struct($1)}
  | CHANNEL '<' flow_type '>' {Channel($3, Nodir)}
  | IN flow_type              {Channel($2, In)}
  | OUT flow_type             {Channel($2, Out)}

var_declaration:
    simple_var_declaration      {$1}
  | init_var_declaration        {$1}

simple_var_declaration:
    flow_type IDENTIFIER {{declaration_type = $1;
                           declaration_id   = $2;
                           declaration_initializer = Noexpr}}

init_var_declaration:
  | flow_type IDENTIFIER ASSIGN expr {{declaration_type = $1;
                                       declaration_id   = $2;
                                       declaration_initializer = $4}}

struct_declaration:
  STRUCT IDENTIFIER LBRACE stmt_list RBRACE {
                                              let decls = List.map (fun e -> match e with
                                                 Declaration(decl) -> decl
                                               | _ -> raise (Failure ("Struct member " ^
                                                            "definitions must be" ^
                                                            "declarations"))) $4 in
                                              {struct_name = $2;
                                               struct_members = decls;
                                              }
                                            }

dot_initializer_list:
    dot_initializer {[$1]}
  | dot_initializer COMMA dot_initializer_list { $1::$3 }

dot_initializer:
    DOT IDENTIFIER ASSIGN expr {{ dot_initializer_id = $2;
                                  dot_initializer_val = $4 }}

stmt_list:
    stmt {[$1]}
  | stmt stmt_list {$1::$2}

stmt:
    expr_stmt        {$1}
  | compound_stmt    {$1}
  | selection_stmt   {$1}
  | iteration_stmt   {$1}
  | var_declaration SEMI {Declaration($1)}
  | jump_stmt        {$1}

expr_stmt:
    expr SEMI {Expr($1)}

compound_stmt:
    LBRACE stmt_list RBRACE {Block($2)}
  | LBRACE RBRACE {Block([])}

selection_stmt:
    IF LPAREN expr RPAREN stmt %prec NOELSE {If($3, $5, Expr(Noexpr))}
  | IF LPAREN expr RPAREN stmt ELSE stmt    {If($3, $5, $7)}

iteration_stmt:
    WHILE LPAREN expr RPAREN stmt {While($3, $5)}
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt {For($3, $5, $7, $9)}

jump_stmt:
    RETURN expr SEMI {Return($2)}
  | RETURN SEMI {Return(Noexpr)}
  | CONTINUE SEMI {Continue}
  | BREAK SEMI {Break}

expr_opt:
    /* nothing */ {Noexpr}
  | expr {$1}

expr_list:
    expr {[$1]}
  | expr COMMA expr_list {$1::$3}

expr:
    INT_LITERAL {IntLiteral($1)}
  | DOUBLE_LITERAL {DoubleLiteral($1)}
  | STRING_LITERAL {StringLiteral($1)}
  | CHAR_LITERAL {CharLiteral($1)}
  | BOOL_LITERAL {BoolLiteral($1)}
  | IDENTIFIER {Id($1)}
  | LBRACE dot_initializer_list RBRACE {StructInitializer($2)}
  | READ_CHANNEL IDENTIFIER {UnaryOp(Retrieve, Id($2))}
  | expr WRITE_CHANNEL IDENTIFIER {BinOp($1, Send, Id($3))}
  | function_call {$1}
  | expr PLUS expr {BinOp($1, Plus, $3)}
  | expr MINUS expr {BinOp($1, Minus, $3)}
  | expr TIMES expr {BinOp($1, Times, $3)}
  | expr DIVIDE expr {BinOp($1, Divide, $3)}
  | expr MODULO expr {BinOp($1, Modulo, $3)}
  | expr EQ expr {BinOp($1, Eq, $3)}
  | expr NEQ expr {BinOp($1, Neq, $3)}
  | expr LT expr {BinOp($1, Lt, $3)}
  | expr GT expr {BinOp($1, Gt,$3)}
  | expr LEQ expr {BinOp($1, Leq,$3)}
  | expr GEQ expr {BinOp($1, Geq,$3)}
  | expr AND expr {BinOp($1, And,$3)}
  | expr OR expr {BinOp($1, Or,$3)}
  | IDENTIFIER ASSIGN expr {BinOp(Id($1), Assign, $3)}
  | LPAREN expr RPAREN {$2}
  | NOT expr %prec UNARY_OP {UnaryOp(Not, $2)}
  | MINUS expr %prec UNARY_OP { UnaryOp(Negate, $2) }

function_call:
    IDENTIFIER LPAREN RPAREN {FunctionCall($1, [])}
  | IDENTIFIER LPAREN expr_list RPAREN {FunctionCall($1, $3)}
