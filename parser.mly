/* Parser for MASL. */

/* Header section. */
%{
	open Ast;;
%}

/* Declaration section. */

/* Declaring tokens. */

/* Separators. */
%token COLON SEMICOLON COMMA LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA EOF

/* Keywords. */
/* Type specifiers. */
%token INT DOUBLE CHAR BOOL CLASS OBJECT FUN VOID
/* Control flow. */
%token IF ELSE FOR WHILE DO CONTINUE BREAK RETURN
/* Object definitions. */
%token STATE
%token THIS

/* Operators. */
/* Arithmetic operators. */
%token PLUS MINUS MULT DIV MOD
/* Logic operators. */
%token AND OR NOT
/* Relational operators. */
%token GT GE EQ NEQ LE LT
/* Assignment operators. */
%token ASSIGN
/* Object manipulations. */
%token DOT AT TRANS

/* Identifiers. */
%token <string> ID

/* Literals (for basic data types). */
%token <int> INT_LITERAL
%token <float> DOUBLE_LITERAL
%token <char> CHAR_LITERAL
%token <bool> BOOL_LITERAL
%token <char list> STRING_LITERAL

/* Associativity and precedence of operators. */
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left GT GE LE LT
%left PLUS MINUS
%left MULT DIV MOD
%left UPLUS UMINUS NOT
%left DOT
%left AT TRANS
%left LPAREN RPAREN
%nonassoc COLON

%start program
%type <Ast.program> program

%%

/* Rule section. */

/* TODO Lists and strings. */

program:
	  stmt_list { Program($1) }

stmt_list:
	  /* Empty. */ { [] }
	| stmt_nonempty_list { List.rev $1 }

stmt_nonempty_list:
	  stmt { [$1] }
	| stmt_nonempty_list stmt { $2 :: $1 }

stmt:
	  SEMICOLON { NoStmt }
		/* Declaration statement. */
	| basic_type_decl SEMICOLON { $1 }
	| func_decl { $1 }
	| class_decl { $1 }
	| object_decl SEMICOLON { $1 }
	  /* Expression statement. */
	| expr SEMICOLON { Expr($1) }
	  /* Compound statement. */
	| comp_stmt { $1 }
	  /* Control flow statement. */
	| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, NoStmt) }
	| IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
	| FOR LPAREN stmt SEMICOLON expr SEMICOLON expr RPAREN stmt { For($3, $5, $7, $9) }
	| FOR LPAREN type_specifier ID COLON expr RPAREN stmt { ForEach($3, $4, $6, $8) }
	| WHILE LPAREN expr RPAREN stmt { While($3, $5) }
	| DO comp_stmt WHILE LPAREN expr RPAREN { DoWhile($2, $5) }
	  /* Jump statement. */
	| CONTINUE SEMICOLON { Continue }
	| BREAK SEMICOLON { Break }
	| RETURN SEMICOLON { Return(NoExpr) }
	| RETURN expr SEMICOLON { Return($2) }

	/* TODO List declaration. */

basic_type_decl:
	  type_specifier basic_init_decl_list { BasicDecl($1, List.rev $2) }

type_specifier:
	  INT { Int }
	| DOUBLE { Double }
	| CHAR { Char }
	| BOOL { Bool }
	| LPAREN param_type_list RPAREN COLON type_specifier { FuncType($5, List.rev $2) }
	| OBJECT ID { Class($2) }
	
	/* TODO List type specifier. */

basic_init_decl_list:
	  basic_init_decl { [$1] }
	| basic_init_decl_list COMMA basic_init_decl { $3 :: $1 }

basic_init_decl:
	  ID { BasicInitDefault($1) }
	| ID ASSIGN expr { BasicInitAssign($1, $3) }

func_decl:
	  FUN ID ASSIGN expr { FuncDecl($2, $4) }
	| type_specifier ID LPAREN param_list RPAREN comp_stmt { FuncDecl($2, FuncLit($1, List.rev $4, $6)) }

param_list:
	  /* Empty. */ { [] }
	| nonempty_param_list { $1 }

nonempty_param_list:
	  param { [$1] }
	| nonempty_param_list COMMA param { $3 :: $1 }

param:
	  type_specifier ID { ($1, $2) }

param_type_list:
	  /* Empty. */ { [] }
	| nonempty_param_list { List.map fst $1 (* TODO Get the type out of the list. *) }
	| nonempty_unnamed_param_type_list { $1 }

nonempty_unnamed_param_type_list:
	  type_specifier { [$1] }
	| nonempty_unnamed_param_type_list COMMA type_specifier { $3 :: $1 }

func_literal:
	  FUN LPAREN param_list RPAREN COLON type_specifier comp_stmt { ($6, $3, $7) }

class_decl:
	  CLASS ID LBRACE state_decl_list stmt_list RBRACE { ClassDecl($2, $4, $5) }

object_decl:
	  OBJECT ID ASSIGN expr { ObjectDecl($2, $4) }

object_literal:
	  type_specifier LBRACE stmt_list RBRACE { ($1, $3) }

state_decl_list:
	  /* Empty. */ { [] }
	| state_decl_list state_decl { $2:: $1 }

state_decl:
	  STATE ID comp_stmt { ($2, $3) }

basic_literal:
	  INT_LITERAL { IntLit($1) }
	| DOUBLE_LITERAL { DoubleLit($1) }
	| CHAR_LITERAL { CharLit($1) }
	| BOOL_LITERAL { BoolLit($1) }
/*	| STRING_LITERAL {  } 
	| LSQBRA list_elems RSQBRA {}
*/
	/* TODO List literal and string literal. */

/*

list_elems:
	  / * Empty * / {}
	| nonempty_list_elems {}

nonempty_list_elems:
	  expr {}
	| nonempty_list_elems COMMA expr {}

*/

/* TODO Assignment expression and left value structure handling. */

expr:
	  ID { Id($1) }
	| basic_literal { BasicLit($1) }
	| func_literal { FuncLit($1) }
	| object_literal { ObjectLit($1) }
	| THIS { This }
	| LPAREN expr RPAREN { $2 }
	| expr LPAREN arg_list RPAREN { FuncCall($1, List.rev $3) }
	| PLUS expr %prec UPLUS { UnaryOp(Plus, $2) }
	| MINUS expr %prec UMINUS { UnaryOp(Minus, $2) }
	| expr MULT expr { BinaryOp($1, Mult, $3) }
	| expr DIV expr { BinaryOp($1, Div, $3) }
	| expr MOD expr { BinaryOp($1, Mod, $3) }
	| expr PLUS expr { BinaryOp($1, Plus, $3) }
	| expr MINUS expr { BinaryOp($1, Minus, $3) }
	| expr GT expr { BinaryOp($1, Gt, $3) }
	| expr GE expr { BinaryOp($1, Ge, $3) }
	| expr EQ expr { BinaryOp($1, Eq, $3) }
	| expr NEQ expr { BinaryOp($1, Neq, $3) }
	| expr LE expr { BinaryOp($1, Le, $3) }
	| expr LT expr { BinaryOp($1, Lt, $3) }
	| NOT expr { UnaryOp(Not, $2) }
	| expr AND expr { BinaryOp($1, And, $3) }
	| expr OR expr { BinaryOp($1, Or, $3) }
	| expr DOT expr { BinaryOp($1, Dot, $3) }
	| expr AT expr { BinaryOp($1, At, $3) }
	| expr TRANS expr { BinaryOp($1, Trans, $3) }
	| expr ASSIGN expr {BinaryOp($1, Assign, $3) }

arg_list:
	  /* Empty */ { [] }
	| nonempty_arg_list { List.rev $1 }

nonempty_arg_list:
	  expr { [$1] }
	| nonempty_arg_list COMMA expr { $3 :: $1 }

comp_stmt:
	  LBRACE stmt_list RBRACE { CompStmt($2) }

%%

(* Trailer section. *)
