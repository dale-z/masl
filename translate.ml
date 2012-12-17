(* Create translation environment: *)
(* create_env parent_env -> child_env *)
(* Check if current position is contained *)
(* in some loop. *)
(* Linked symbol table. *)
(* *)

open Ast;;
open Str;;

(* translate: string -> Ast.program -> (string * string) list *)

let rec translate sim_name node =
  	match node with
  	| Program(stmts) ->
				(* A MASL program is actually translated into *)
				(* a subclass of MaslSimulation in Java. *)
				"public class " ^ sim_name ^ " extends MaslSimulation {\n" ^
				(* Make the definition of all MASL classes as nested Java class,*)
				(* and make all MASL functions as Java class method. *)
				(List.fold_left
					(fun acc stmt -> acc ^ (translate_stmt "  " stmt)) ""
  					(List.filter
							(fun stmt -> match stmt with
    						| ClassDecl(_, _, _) -> true
								| FuncDecl(_, _, _) -> true
    						| _ -> false) stmts)) ^
				"  public void init() {\n" ^
				(* Everything other than class definition goes into *)
				(* the method init(). *)
				(List.fold_left
					(fun acc stmt -> acc ^ (translate_stmt "    " stmt)) ""
  					(List.filter
							(fun stmt -> match stmt with
    						| ClassDecl(_, _, _) -> false
								| FuncDecl(_, _, _) -> false
    						| _ -> true) stmts)) ^
				"  }\n" ^
				"}\n"
and translate_type_spec node = match node with
	| Int -> "Integer"
	| Double -> "Double"
	| Bool -> "Boolean"
	| Char -> "Char"
	| FuncType(return_type, param_types) -> "MaslFunction<" ^ 
    (translate_type_spec return_type) ^ ">"
	(*| Class(id) -> "class " ^ id*)
	| Class(id) -> id
	(*| Object -> "object"*)
  | ListType(type_spec) -> "MaslList<" ^ (translate_type_spec type_spec) ^ ">"
	| Void -> "void"
and translate_stmt indent node = match node with
	(* decl_stmt *)
  | BasicDecl(type_spec, decl_list) ->
		let str =
			(List.fold_left
				(fun acc decl -> acc ^ (translate_decl type_spec decl) ^ ",")
				"" decl_list) in
		let decls = String.sub str 0 (String.length str - 1) in
		indent ^ translate_type_spec type_spec ^ " " ^ decls ^
		";\n"
  | FuncDecl(type_spec, id, expr) -> let return_type = 
    begin 
      match type_spec with 
      | FuncType(rt,_) -> rt 
      | _ -> Void (*Impossible, used to suppress warning*)
    end 
    in "MaslFunction<" ^ (translate_type_spec return_type) ^ "> " ^ 
    id ^ "=" ^ (translate_expr expr)
	| ClassDecl(id, states, stmts) -> indent ^ "public class " ^ 
    id ^ " extends MaslClass {\n" ^ (generate_state_update states) ^
    (translate_states states) ^
    (* Add "public" before each statement*) 
    (List.fold_left
        (fun acc stmt -> acc ^ "\npublic " ^ (translate_stmt "  " stmt))
        "" stmts
    ) ^ "}\n"
  (*| ObjectDecl(id, expr) -> indent ^ "objectdecl\n"*)
	(* expr_stmt *)
  | Expr(expr) -> indent ^ translate_expr expr ^ ";\n"
	(* comp_stmt *)
  | CompStmt(stmts) ->
		indent ^ "{\n" ^
		(List.fold_left
				(fun acc stmt -> acc ^ (translate_stmt ("  " ^ indent) stmt))
				"" stmts) ^
		indent ^ "}\n"
	(* control_flow_stmt *)
  | If(pred, then_body, else_body) ->
		indent ^ "if(" ^ translate_expr pred ^ ") {\n" ^
		translate_stmt ("  " ^ indent) then_body ^
		indent ^ "} else {\n" ^
		translate_stmt ("  " ^ indent) else_body ^
		indent ^ "}\n"
  | For(init, pred, update, body) ->
		indent ^ "for(" ^ translate_stmt "" init ^ ";" ^
		translate_expr pred ^ ";" ^
		translate_expr update ^ ") {\n" ^
		translate_stmt ("  " ^ indent) body ^
		indent ^ "}\n"
  | ForEach(type_spec, iter, container, body) -> indent ^ "#ForEach#\n"
  | While(pred, body) -> indent ^ "while(" ^ 
		translate_expr pred ^ ") {\n" ^
		translate_stmt ("  " ^ indent) body ^ "}\n"
  | DoWhile(body, pred) -> indent ^ "do {" ^ translate_stmt ("  " ^ indent) body ^ "}while(" ^ translate_expr pred ^ ");\n"
	(* jump_stmt *)
  | Continue -> indent ^ "continue;\n"
  | Break -> indent ^ "break;\n"
  | Return(expr) -> indent ^ "return " ^ translate_expr expr ^ ";\n"
  | NoStmt -> indent ^ ""
and translate_expr node =
	match node with
	|	Id(id) -> id
	| BasicLit(lit) ->
		begin
  		match lit with
  		| IntLit(lit) -> string_of_int lit
  		| DoubleLit(lit) -> string_of_float lit
  		| CharLit(lit) -> Char.escaped lit
  		| BoolLit(lit) -> string_of_bool lit
		| ObjectLit(lit) -> "ObjectLit"
		| ListLit(type_spec, exprs) -> "ObjectLit"
		end
	| FuncLit(type_spec, param_list, comp_stmt) -> "new MaslFunction<" ^ (translate_type_spec type_spec) ^ 
        ">() {\n@Override\npublic " ^ (translate_type_spec type_spec) ^
        " invoke(Object... args) {\n" ^
        let idxs = List.mapi (fun idx ele -> idx) param_list in
        (List.fold_left2
          (fun acc param idx -> acc ^ (translate_type_spec (fst param)) ^ " " ^ (snd param) ^ 
          " = (" ^ (translate_type_spec (fst param) ^ ") args[" ^ (string_of_int idx) ^ "];\n")
          )
          "" param_list idxs
        ) ^
        (translate_stmt "  " comp_stmt)
	(*| ObjectLit(lit) -> "ObjectLit"*)
	| This -> "this"
	| UnaryOp(op, expr) ->
		"(" ^
		begin
			match op with
			| Plus -> "+"
			| Minus -> "-"
			| Not -> "!"
			| _ -> ""
		end ^
		translate_expr expr ^ ")";
	| BinaryOp(expr1, op, expr2) ->
		"(" ^ (translate_expr expr1) ^
		begin
			match op with
			| Plus -> "+"
			| Minus -> "-"
			| Mult -> "*"
			| Div -> "/"
			| Mod -> "%"
			| And -> "&&"
			| Or -> "||"
			| Gt -> ">"
			| Ge -> ">="
			| Eq -> "=="
			| Neq -> "!=" 
			| Le -> "<="
			| Lt -> "<"
			| Assign -> "="
			| Dot -> "."
			| At -> "#At#"
			| Trans -> "#Trans#"
			| _ -> ""
		end ^
		(translate_expr expr2) ^ ")"
	| FuncCall(func, args) -> (translate_expr func) ^ ".invoke(" ^ (translate_arg_list args) ^ ");"
	| NoExpr -> ""
and translate_decl type_spec decl = match decl with
	  BasicInitDefault(id) ->
		id ^ "=" ^
		begin
  		match type_spec with
    	| Int -> "0"
    	| Double -> "0.0"
    	| Bool -> "false"
    	| Char -> "'\\0'"
    	| FuncType(return_type, param_types) -> "null"
    	| Class(id) -> "new " ^ id ^ "()"
    	| ListType(type_spec) -> "new MaslList<" ^ (translate_type_spec type_spec) ^ ">();"
    	(*| Object -> "#ObjectDefaultValue#"*)
    	| Void -> "#VoidDefaultValue#"
		end
  | BasicInitAssign(id, expr) ->
		id ^ "=" ^ translate_expr expr
and translate_states states = 
  (List.fold_left
			(fun acc state -> acc ^ "private void " ^ (fst state) ^ "() {\n" ^ 
      translate_stmt "  " (snd state) ^ "}\n")
				"" states
	)

(*Generate __update function*)
and generate_state_update states = "public void __update() {\n" ^ 
		(List.fold_left
			(fun acc state -> acc ^ "case \"" ^ (fst state) ^ "\":" ^ 
      (fst state) ^ "();break;\n")
				"" states
    ) ^ "default:\nbreak;\n}\n"
and translate_arg_list arg_list = let str = 
  (List.fold_left 
    (fun acc arg -> acc ^ "," ^ (translate_expr arg))
    "" arg_list
  ) in replace_first (regexp ",") "" str
;;
