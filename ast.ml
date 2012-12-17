(*** Tokens. ***)


(* Operators. *)
type op =
	  Plus | Minus | Mult | Div | Mod
	| And | Or | Not
	| Gt | Ge | Eq | Neq | Le | Lt
	| Assign
	| Dot | At | Trans | Index | LDot

(* Basic type literals. *)
and basic_literal =
	  IntLit of int
	| DoubleLit of float
	| CharLit of char
	| BoolLit of bool
	| ObjectLit of object_literal
	| ListLit of type_spec * expr list

(*** Productions. ***)

(* Type specifiers. *)
and type_spec =
	  Int
	| Double
	| Char
	| Bool
	| Void
	| Class of string
	| FuncType of type_spec * type_spec list
	| ListType of type_spec

(* Expressions. *)
and expr =
		Id of string
	| BasicLit of basic_literal
	| FuncLit of func_literal
	| This
	| UnaryOp of op * expr
	| BinaryOp of expr * op * expr
	| FuncCall of expr * expr list
	| NoExpr

(* Statements. *)

(* Basic type declaration. *)
and basic_init_decl =
	  BasicInitDefault of string
	| BasicInitAssign of string * expr

(* Function declaration. *)
and param = type_spec * string
and func_literal = type_spec * param list * stmt

(* Object declaration. *)
and state = string * stmt
and object_literal = type_spec

(* Overall structures of statements. *)
and stmt =
(* Declaration statements. *)
	| BasicDecl of type_spec * basic_init_decl list
	| FuncDecl of type_spec * string * expr
	| ClassDecl of string * state list * stmt list
(* Expression statement. *)
	| Expr of expr
(* Compound statement. *)
	| CompStmt of stmt list
(* Control flow statement. *)
	| If of expr * stmt * stmt
	| For of stmt * expr * expr * stmt
	| ForEach of  type_spec * string * expr * stmt
	| While of expr * stmt
	| DoWhile of stmt * expr
(* Jump statement. *)
	| Continue
	| Break
	| Return of expr
	| NoStmt

(* input *)
and program = Program of stmt list;;