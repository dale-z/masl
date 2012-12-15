(*** Tokens. ***)


(* Operators. *)
type op =
	  Plus | Minus | Mult | Div | Mod
	| And | Or | Not
	| Gt | Ge | Eq | Neq | Le | Lt
	| Assign
	| Dot | At | Trans;;

(* Basic type literals. *)
type basic_literal =
	  IntLit of int
	| DoubleLit of float
	| CharLit of char
	| BoolLit of bool;;

(*** Productions. ***)

(* Type specifiers. *)
type type_spec =
	  Int
	| Double
	| Char
	| Bool
	| Void
	| Class of string
	| Object
	| FuncType of type_spec * type_spec list;;

(* Expressions. *)
type expr =
		Id of string
	| BasicLit of basic_literal
	| FuncLit of func_literal
	| ObjectLit of object_literal
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
and object_literal = type_spec * stmt list

(* Overall structures of statements. *)
and stmt =
(* Declaration statements. *)
	| BasicDecl of type_spec * basic_init_decl list
	| FuncDecl of string * expr
	| ClassDecl of string * state list * stmt list
	| ObjectDecl of string * expr
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
	| NoStmt;;

(* TODO Lists and strings. *)

(* input *)
type program = Program of stmt list;;