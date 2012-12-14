(*** Tokens. ***)

(* 1. Separators: They do not appear in an AST. *)

(* 2. Keywords. *)
(* Type specifiers: Refer to declaration statement productions. *)
(* Control flow: The tokens appear in their productions. *)
(* Object definitions: THIS goes to expr. *)
(* STATE appears in the productions. *)

(* 3. Operators. *)
type op =
	  Plus | Minus | Mult | Div | Mod
	| And | Or | Not
	| Gt | Ge | Eq | Neq | Le | Lt
	| Assign
	| Dot | At | Trans;;

(* 4. Identifiers: Their tokens are all of string type. *)

(* 5. Literals. *)
type basic_literal =
	  IntLit of int
	| DoubleLit of float
	| CharLit of char
	| BoolLit of bool;;

(*** Productions. ***)

type type_spec =
	  Int
	| Double
	| Char
	| Bool
	| Void
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
and object_literal = string * state list * stmt list

(* Overall structures of statements. *)
and stmt =
(* decl_stmt *)
	| BasicDecl of type_spec * basic_init_decl list
	| FuncDecl of string * func_literal
	| ObjectDecl of string * object_literal
(* expr_stmt *)
	| Expr of expr
(* comp_stmt *)
	| CompStmt of stmt list
(* control_flow_stmt *)
	| If of expr * stmt * stmt
	| For of stmt * expr * expr * stmt
	| ForEach of  type_spec * string * expr * stmt
	| While of expr * stmt
	| DoWhile of stmt * expr
(* jump_stmt *)
	| Continue
	| Break
	| Return of expr
	| NoStmt;;

(* TODO Lists and strings. *)

(* input *)
type program = Program of stmt list;;

(* A utility that prints out the AST. *)

let rec print_program node = match node with
	| Program(stmts) ->
		print_string "Program {\n";
		List.iter (print_stmt "  ") stmts;
		print_string "}\n"
and print_type_spec indent node = match node with
	| Int -> print_string (indent ^ "Int\n")
	| Double -> print_string (indent ^ "Double\n")
	| Bool -> print_string (indent ^ "Bool\n")
	| Char -> print_string (indent ^ "Char\n")
	| FuncType(return_type, param_types) ->
		print_string (indent ^ "FuncType {\n");
		print_type_spec ("  " ^ indent) return_type;
		List.iter
				(print_type_spec ("  " ^ indent)) param_types;
		print_string (indent ^ "}\n")
	| Object -> print_string (indent ^ "Object\n")
	| Void -> print_string (indent ^ "Void\n")
and print_stmt indent node = match node with
  | BasicDecl(type_spec, decl_list) ->
    	print_string (indent ^ "BasicDecl {\n");
			print_type_spec ("  " ^ indent) type_spec;
    	List.iter (print_basic_init_decl ("  " ^ indent)) decl_list;
			print_string (indent ^ "}\n")
  | FuncDecl(id, lit) ->
		print_string (indent ^ "FuncDecl {\n");
		print_string ("  " ^ indent ^ "_Id {" ^ id ^ "}\n");
		print_func_lit ("  " ^ indent) lit;
		print_string (indent ^ "}\n")
  | ObjectDecl(id, lit) ->
		print_string (indent ^ "ObjectDecl {\n");
		print_string ("  " ^ indent ^ " _Id {" ^ id ^ "}\n");
		print_object_lit ("  " ^ indent) lit;
		print_string (indent ^ "}\n")
  | Expr(expr) ->
		print_string (indent ^ "Expr {\n");
		print_expr ("  " ^ indent) expr;
		print_string (indent ^ "}\n")
  | CompStmt(stmts) ->
		print_string (indent ^ "CompStmt {\n");
		List.iter (print_stmt ("  " ^ indent)) stmts;
		print_string (indent ^ "}\n")
  | If(pred, then_body, else_body) ->
		print_string (indent ^ "If {\n");
		print_expr ("  " ^ indent) pred;
		print_stmt ("  " ^ indent) then_body;
		print_stmt ("  " ^ indent) else_body;
		print_string (indent ^ "}\n")
  | For(init, pred, update, body) ->
		print_string (indent ^ "For {\n");
		print_stmt ("  " ^ indent) init;
		print_expr ("  " ^ indent) pred;
		print_expr ("  " ^ indent) update;
		print_stmt ("  " ^ indent) body;
		print_string (indent ^ "}\n")
  | ForEach(type_spec, iter, container, body) ->
		print_string (indent ^ "ForEach {\n");
		print_type_spec ("  " ^ indent) type_spec;
		print_string ("  " ^ indent ^ iter);
		print_expr ("  " ^ indent) container;
		print_stmt ("  " ^ indent) body;
		print_string (indent ^ "}\n")
  | While(pred, body) ->
		print_string (indent ^ "While {\n");
		print_expr ("  " ^ indent) pred;
		print_stmt ("  " ^ indent) body;
		print_string (indent ^ "}\n")
  | DoWhile(body, pred) ->
		print_string (indent ^ "DoWhile {\n");
		print_expr ("  " ^ indent) pred;
		print_stmt ("  " ^ indent) body;
		print_string (indent ^ "}\n")
  | Continue -> print_string (indent ^ "Continue\n")
  | Break -> print_string (indent ^ "Break\n")
  | Return(expr) ->
		print_string (indent ^ "Return {\n");
		print_expr ("  " ^ indent) expr;
		print_string (indent ^ "}\n")
  | NoStmt -> print_string (indent ^ "NoStmt\n")
and print_expr indent node = match node with
	|	Id(id) -> print_string (indent ^ "Id {" ^ id ^ "}\n")
	| BasicLit(lit) ->
		print_string (indent ^ "BasicLit {\n");
		begin
  		match lit with
  		| IntLit(lit) ->
				print_string ("  " ^ indent ^ "IntLit {" ^ (string_of_int lit) ^ "}\n")
  		| DoubleLit(lit) -> print_string ("  " ^ indent ^ "DoubleLit {" ^ (string_of_float lit) ^ "}\n")
  		| CharLit(lit) -> print_string ("  " ^ indent ^ "CharLit {" ^ (Char.escaped lit) ^ "}\n")
  		| BoolLit(lit) -> print_string ("  " ^ indent ^ "BoolLit {" ^ (string_of_bool lit) ^ "}\n")
		end
	| FuncLit(lit) ->
		print_string (indent ^ "FuncLit {\n");
		print_func_lit ("  " ^ indent) lit;
		print_string (indent ^ "}\n")
	| ObjectLit(lit) ->
		print_string (indent ^ "ObjectLit {\n");
		print_object_lit ("  " ^ indent) lit;
		print_string (indent ^ "}\n")
	| This -> print_string (indent ^ "This\n")
	| UnaryOp(op, expr) ->
		begin
			match op with
			| Plus -> print_string (indent ^ "Plus {\n");
			| Minus -> print_string (indent ^ "Minus {\n");
			| Not -> print_string (indent ^ "Not {\n");
			| _ -> ()
		end;
		print_expr ("  " ^ indent) expr;
		print_string (indent ^ "}\n")
	| BinaryOp(expr1, op, expr2) ->
		begin
			match op with
			| Plus -> print_string (indent ^ "Plus {\n")
			| Minus -> print_string (indent ^ "Minus {\n")
			| Mult -> print_string (indent ^ "Mult {\n")
			| Div -> print_string (indent ^ "Div {\n")
			| Mod -> print_string (indent ^ "Mod {\n")
			| And -> print_string (indent ^ "And {\n")
			| Or -> print_string (indent ^ "Or {\n")
			| Gt -> print_string (indent ^ "Gt {\n")
			| Ge -> print_string (indent ^ "Ge {\n")
			| Eq -> print_string (indent ^ "Eq {\n")
			| Neq -> print_string (indent ^ "Neq {\n") 
			| Le -> print_string (indent ^ "Le {\n")
			| Lt -> print_string (indent ^ "Lt {\n")
			| Assign -> print_string (indent ^ "Assign {\n")
			| Dot -> print_string (indent ^ "Dot {\n")
			| At -> print_string (indent ^ "At {\n")
			| Trans -> print_string (indent ^ "Trans {\n")
			| _ -> ()
		end;
		print_expr ("  " ^ indent) expr1;
		print_expr ("  " ^ indent) expr2;
		print_string (indent ^ "}\n")
	| FuncCall(func, args) ->
		print_string (indent ^ "FuncCall {\n");
		print_expr ("  " ^ indent) func;
		List.iter (print_expr ("  " ^ indent)) args;
		print_string (indent ^ "}\n")
	| NoExpr -> print_string (indent ^ "NoExpr");
and print_basic_init_decl indent node = match node with
  | BasicInitDefault(id) ->
		print_string (indent ^ "BasicInitDefault {\n");
		print_string ("  " ^ indent ^ id ^ "\n");
		print_string (indent ^ "}\n")
  | BasicInitAssign(id, expr) ->
		print_string (indent ^ "BasicInitAssign {\n");
		print_string ("  " ^ indent ^ id ^ "\n");
		print_expr ("  " ^ indent) expr;
		print_string (indent ^ "}\n")
and print_func_lit indent node = match node with
	| (return_type, params, body) ->
		print_string (indent ^ "_ReturnType {\n");
		print_type_spec ("  " ^ indent) return_type;
		print_string (indent ^ "}\n");
		List.iter
			(fun (param_type, param_id) ->
				print_string (indent ^ "_Arg {\n");
				print_type_spec ("  " ^ indent) param_type;
				print_string ("  " ^ indent ^ param_id ^ "\n");
				print_string (indent ^ "}\n"))
			params;
		print_stmt indent body
and print_state indent node =
	print_string (indent ^ "_State {\n");
	begin
  	match node with
  	| (state_id, body) ->
  		print_string ("  " ^ indent ^ state_id);
  		print_stmt ("  " ^ indent) body
	end;
	print_string (indent ^ "}\n")
and print_object_lit indent node = match node with
	| (base_obj, states, stmts) ->
		print_string (indent ^ base_obj);
		List.iter (print_state indent) states;
		List.iter (print_stmt indent) stmts;
;;