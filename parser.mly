%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA
%token SHIFT_RIGHT SHIFT_LEFT BIT_OR BIT_NOT BIT_AND BIT_XOR
%token PLUS MINUS TIMES DIVIDE MODULO ASSIGN
%token WRITE_CHANNEL READ_CHANNEL PROC CHANNEL IN OUT
%token DOT
%token BREAK CONTINUE VOID
%token OR AND NEGATE
%token DOUBLE CHAR BOOL INT STRING LIST STRUCT 
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE INT
%token <int> INT_LITERAL
%token <float> DOUBLE_LITERAL
%token <char> CHAR_LITERAL
%token <bool> BOOL_LITERAL
%token <string> STRING_LITERAL
%token <string> IDENTIFIER
%token EOF

%nonassoc NOELSE /* wtf is this */
%nonassoc ELSE
%right ASSIGN
%left AND OR
%right BIT_AND BIT_OR BIT_XOR
%left EQ NEQ
%left LT GT LEQ GEQ
%right SHIFT_LEFT SHIFT_RIGHT
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right BIT_NOT NEGATE
%left WRITE_CHANNEL READ_CHANNEL
%left DOT

%start program
%type <Ast.program> program

%%

/*program:
    declaration-stmt { $1 }
  | function_declaration { $1 }
  | process_declaration { $1 }
  | program program { $1 }
  | program EOF { $1 }
*/

program:
  decls EOF { Todo }

decls:
    /* nothing */ { Todo }
  | decls declaration_stmt { Todo }
  | decls function_declaration { Todo }
  | decls process_declaration { Todo }
/*| decls vdecl { ($2 :: fst $1), snd $1 }
  | decls fdecl { fst $1, ($2 :: snd $1) }*/

declaration_stmt:
    primitive_declaration {Todo}
  | string_declaration {Todo}
  | list_declaration {Todo}
  | struct_declaration {Todo}
  | struct_instance_declaration {Todo}
  | channel_declaration {Todo}

function_declaration:
    function_declarator SEMI {Todo}
  | function_declarator LBRACE RBRACE {Todo}
  | function_declarator LBRACE stmt_list RBRACE {Todo}

function_declarator:
    primitive_type IDENTIFIER LPAREN RPAREN {Todo}
  | primitive_type IDENTIFIER LPAREN arg_declaration_list RPAREN {Todo} 

process_declaration:
    process_declarator SEMI {Todo}
  | process_declarator LBRACE RBRACE {Todo}
  | process_declarator LBRACE stmt_list RBRACE {Todo}

process_declarator:
    PROC IDENTIFIER LPAREN arg_declaration_list RPAREN {Todo}

primitive_declaration:
    primitive_declarator SEMI {Todo}
  | primitive_declarator ASSIGN expr SEMI {Todo}

primitive_declarator:
    primitive_type IDENTIFIER {Todo}

primitive_type:
    INT {Todo}
  | DOUBLE {Todo}
  | CHAR {Todo}
  | BOOL {Todo}
  | VOID {Todo}

string_declaration:
    string_declarator SEMI {Todo}
  | string_declarator ASSIGN STRING_LITERAL SEMI {Todo}

string_declarator:
    STRING IDENTIFIER {Todo}

list_declarator: 
    list_type LIST IDENTIFIER { Todo }

list_type:
    primitive_type {Todo}
  | STRING {Todo}
  | IDENTIFIER {Todo}

list_declaration:
    list_declarator SEMI {Todo}
  | list_declarator ASSIGN list_initializer SEMI {Todo}

list_initializer: 
    LBRACKET expr_list RBRACKET {Todo}

struct_declaration:
    STRUCT IDENTIFIER LBRACE struct_member_list RBRACE {Todo}

struct_member_list:
    struct_member_declarator {Todo}
  | struct_member_list COMMA struct_member_declarator {Todo}

struct_member_declarator:
    primitive_declarator {Todo}
  | string_declarator {Todo}
  | list_declarator {Todo}
  | struct_instance_declarator {Todo}

struct_instance_declarator:
    IDENTIFIER IDENTIFIER {Todo}

struct_instance_declaration:
    struct_instance_declarator SEMI {Todo}
  | struct_instance_declarator ASSIGN LBRACE dot_initializer_list RBRACE {Todo}

dot_initializer_list:
    DOT IDENTIFIER ASSIGN expr {Todo}
  | DOT IDENTIFIER ASSIGN expr COMMA dot_initializer_list {Todo}

channel_declaration: 
    channel_type CHANNEL IDENTIFIER SEMI {Todo}

channel_type:
    primitive_type {Todo}
  | STRING {Todo}
  | IDENTIFIER {Todo}

stmt_list:
    stmt {Todo}
  | stmt stmt_list {Todo}

stmt:
    expr_stmt {Todo}
  | compound_stmt {Todo}
  | selection_stmt {Todo}
  | iteration_stmt {Todo}
  | declaration_stmt {Todo}
  | jump_stmt {Todo}

expr_stmt: 
    expr SEMI {Todo}

compound_stmt:
    LBRACE stmt_list RBRACE {Todo}

selection_stmt:
    IF LPAREN expr RPAREN stmt %prec NOELSE {Todo}
  | IF LPAREN expr RPAREN stmt ELSE stmt {Todo}

jump_stmt:
    RETURN expr SEMI {Todo}
  | RETURN SEMI {Todo}
  | CONTINUE SEMI {Todo}
  | BREAK SEMI {Todo}

expr_opt:
    /* nothing */ {Todo} 
  | expr {Todo}

iteration_stmt:
    WHILE LPAREN expr RPAREN stmt {Todo}
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt {Todo}

arg_declaration_list:
    /* nothing */
  | arg_declaration {Todo}
  | arg_declaration_list COMMA arg_declaration {Todo}

arg_declaration:
    primitive_declarator {Todo}
  | list_declarator {Todo}
  | IDENTIFIER IDENTIFIER {Todo}
  | IN channel_type IDENTIFIER {Todo}
  | OUT channel_type IDENTIFIER {Todo}

expr_list:
    expr {Todo}
  | expr COMMA expr_list {Todo}

expr:
    INT_LITERAL {Todo}  
  | DOUBLE_LITERAL {Todo}
  | STRING_LITERAL {Todo}
  | CHAR_LITERAL {Todo}
  | BOOL_LITERAL {Todo}
  | IDENTIFIER {Todo}
  | READ_CHANNEL IDENTIFIER {Todo}
  | expr WRITE_CHANNEL IDENTIFIER {Todo}
  | function_call {Todo}
  | expr PLUS expr {Todo}
  | expr MINUS expr {Todo}
  | expr TIMES expr {Todo}
  | expr DIVIDE expr {Todo}
  | expr MODULO expr {Todo}
  | expr EQ expr {Todo}
  | expr NEQ expr {Todo}
  | expr LT expr {Todo}
  | expr GT expr {Todo}
  | expr LEQ expr {Todo}
  | expr GEQ expr {Todo}
  | expr SHIFT_LEFT expr {Todo}
  | expr SHIFT_RIGHT expr {Todo}
  | expr BIT_XOR expr {Todo}
  | expr BIT_AND expr {Todo}
  | expr BIT_OR expr {Todo}
  | expr AND expr {Todo}
  | expr OR expr {Todo}
  | IDENTIFIER ASSIGN expr {Todo}
  | IDENTIFIER LBRACKET INT_LITERAL RBRACKET ASSIGN expr {Todo}
  | IDENTIFIER LBRACKET INT_LITERAL RBRACKET {Todo}
  | LPAREN expr RPAREN {Todo}
  | BIT_NOT expr {Todo}
  | NEGATE expr {Todo}

function_call:
    IDENTIFIER LPAREN RPAREN {Todo}
  | IDENTIFIER LPAREN expr_list RPAREN {Todo}
