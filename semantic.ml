(*Semantic Check*)
open Ast;;
module NameMap = Map.Make(struct
	type t = string
	let compare x y = Pervasives.compare x y 
end)

(*check program*)

let rec check_semantic program =	 
	match program with
	| Program(stmt_list) -> List.fold_left (check_stmt) NameMap.empty stmt_list

(*check statements*)

and check_stmt v_table stmt = 
	match stmt with (*match all types of statements*)
	| BasicDecl(type_spec, basic_init_decl) -> 
		let rec check_basic_init_decl v_table list =
			match list with
			| [] -> v_table
			| head::tail -> 
  			match head with
  			| BasicInitDefault(id) -> check_basic_init_decl (NameMap.add id type_spec v_table) tail	 
  			| BasicInitAssign(id, expr) ->
  				if(check_expr v_table type_spec expr) then
  					check_basic_init_decl 
						(
							if NameMap.mem id v_table then
								v_table
							else
								NameMap.add id type_spec v_table
						) 
						tail
  				else
  					raise (Failure("Basic Assignment Check Fails\n"))
		in check_basic_init_decl v_table basic_init_decl				 
	| _ -> raise (Failure("Basic Declaration Fails\n"))

(*check expression*)

and check_expr v_table type_spec expr = 
	match expr with (*match all types of expressions*)
	| Id(id) -> 
		if NameMap.mem id v_table then
			match (type_spec) with
			| Int -> ((NameMap.find id v_table) = Int) || ((NameMap.find id v_table) = Char)
			| Double -> ((NameMap.find id v_table) = Int) || ((NameMap.find id v_table) = Char) || ((NameMap.find id v_table) = Double)
			| els -> (NameMap.find id v_table) = els
		else
			raise (Failure("Cannot Find Identifier.\n"))
	| BasicLit(basic_literal) ->
		(
		match basic_literal with
		| IntLit(t) -> (type_spec = Int) || (type_spec = Double)
		| DoubleLit(t) -> type_spec = Double
		| CharLit(t) ->  (type_spec = Int) || (type_spec = Char) || (type_spec = Double)
		| BoolLit(t) -> type_spec = Bool
		)
	| FuncLit(func_literal) -> false (*to be continued*)
	| ObjectLit(object_literal) -> false (*to be continued*)
	| This -> false (*to be continued*)
	| UnaryOp(op, expr) ->
		( 
		match op with
		| Plus -> check_expr v_table Double expr
		| Minus -> check_expr v_table Double expr
		| Not -> check_expr v_table Bool expr
		)
	| BinaryOp(e1, op, e2) ->
		( 
		match op with
		| Plus -> (check_expr v_table Double e1) && (check_expr v_table Double e2)
		| Minus -> (check_expr v_table Double e1) && (check_expr v_table Double e2)
		| Mult -> (check_expr v_table Double e1) && (check_expr v_table Double e2)
		| Div -> (check_expr v_table Double e1) && (check_expr v_table Double e2)
		| Mod -> (check_expr v_table Int e1) && (check_expr v_table Int e2)
		| And -> (check_expr v_table Bool e1) && (check_expr v_table Bool e2)
		| Or -> (check_expr v_table Bool e1) && (check_expr v_table Bool e2)
		| Gt -> (check_expr v_table Double e1) && (check_expr v_table Double e2)
		| Ge -> (check_expr v_table Double e1) && (check_expr v_table Double e2)
		| Eq -> (check_expr v_table Double e1) && (check_expr v_table Double e2)
		| Neq -> (check_expr v_table Double e1) && (check_expr v_table Double e2)
		| Le -> (check_expr v_table Double e1) && (check_expr v_table Double e2)
		| Lt -> (check_expr v_table Double e1) && (check_expr v_table Double e2)
		| Assign -> false (*too complicated*)
		| Trans -> false (*?????*)
		| At -> false (*???*)
		| Dot -> false (*?????*)
		)
	| FuncCall -> false
	| NoExpr -> true;
;;
