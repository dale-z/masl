(*Semantic Check*)
open Ast;;

module NameMap = Map.Make(struct
	type t = string
	let compare x y = Pervasives.compare x y 
end)


(*check program*)

(*Environment Details:*)
(*0: Outer Environment*)
(*1: class*)
(*2: Loop*)
(*3: if*)

let rec check_semantic program =	 
	match program with
	| Program(stmt_list) -> List.fold_left (check_stmt 0(*outer env*)) (NameMap.empty, NameMap.empty, NameMap.empty) stmt_list

(*check statements*)

and check_stmt env (v_table, c_table, s_table) stmt = 
	match stmt with (*match all types of statements*)
	| BasicDecl(type_spec, basic_init_decl) -> 
		let rec check_basic_init_decl v_table list = (*recursively check the basic declaration list*)
			match list with
			| [] -> (v_table, c_table, s_table)
			| head::tail -> 
  			match head with
  			| BasicInitDefault(id) -> check_basic_init_decl (NameMap.add id type_spec v_table) tail
  			| BasicInitAssign(id, expr) ->
  				if(check_expr v_table c_table s_table env type_spec expr) then
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
	| Expr(expr) -> 
		if check_expr v_table c_table s_table env Void expr then
			(v_table, c_table, s_table)
		else
			raise (Failure("Statement Error"))
	| _ -> raise (Failure("Not Finished"))

(*check expression*)

and check_expr v_table c_table s_table env type_spec expr = 
	match expr with (*match all types of expressions*)
	| Id(id) -> 
		if NameMap.mem id v_table then
			match (type_spec) with
			| Int -> ((NameMap.find id v_table) = Int) || ((NameMap.find id v_table) = Char)
			| Double -> ((NameMap.find id v_table) = Int) || ((NameMap.find id v_table) = Char) || ((NameMap.find id v_table) = Double)
			| els -> (NameMap.find id v_table) = els
		else
			raise (Failure("Cannot Find Identifier."))
	| BasicLit(basic_literal) ->
		(
		match basic_literal with
		| IntLit(t) -> (type_spec = Int) || (type_spec = Double)
		| DoubleLit(t) -> type_spec = Double
		| CharLit(t) ->  (type_spec = Int) || (type_spec = Char) || (type_spec = Double)
		| BoolLit(t) -> type_spec = Bool
		| ObjectLit(object_literal) ->
			(
			match object_literal with
			| Class(id) ->
				if NameMap.mem id c_table then
					true
				else
					raise (Failure("Cannot Find Class "^id))
			| _ -> false
			)
		| ListLit(type_spec, expr_list) -> false (*??????????????????????*)
		)
	| FuncLit(func_literal) -> false (*????????????????????*)
	| This ->
		if env = 1 then
			true
		else
			false
	| UnaryOp(op, expr) ->
		( 
		match op with
		| Plus -> check_expr v_table c_table s_table env Double expr
		| Minus -> check_expr v_table c_table s_table env Double expr
		| Not -> check_expr v_table c_table s_table env Bool expr
		)
	| BinaryOp(e1, op, e2) ->
		( 
		match op with
		| Plus -> (check_expr v_table c_table s_table env Double e1) && (check_expr v_table c_table s_table env Double e2)
		| Minus -> (check_expr v_table c_table s_table env Double e1) && (check_expr v_table c_table s_table env Double e2)
		| Mult -> (check_expr v_table c_table s_table env Double e1) && (check_expr v_table c_table s_table env Double e2)
		| Div -> (check_expr v_table c_table s_table env Double e1) && (check_expr v_table c_table s_table env Double e2)
		| Mod -> (check_expr v_table c_table s_table env Int e1) && (check_expr v_table c_table s_table env Int e2)
		| And -> (check_expr v_table c_table s_table env Bool e1) && (check_expr v_table c_table s_table env Bool e2)
		| Or -> (check_expr v_table c_table s_table env Bool e1) && (check_expr v_table c_table s_table env Bool e2)
		| Gt -> (check_expr v_table c_table s_table env Double e1) && (check_expr v_table c_table s_table env Double e2)
		| Ge -> (check_expr v_table c_table s_table env Double e1) && (check_expr v_table c_table s_table env Double e2)
		| Eq -> (check_expr v_table c_table s_table env Double e1) && (check_expr v_table c_table s_table env Double e2)
		| Neq -> (check_expr v_table c_table s_table env Double e1) && (check_expr v_table c_table s_table env Double e2)
		| Le -> (check_expr v_table c_table s_table env Double e1) && (check_expr v_table c_table s_table env Double e2)
		| Lt -> (check_expr v_table c_table s_table env Double e1) && (check_expr v_table c_table s_table env Double e2)
		| Assign -> 
			let check_left_type el =
				(*only when the left is an identifier, A.B or A:[B] can the assignment success*)
				match el with
				| Id(id) -> 
					if (NameMap.mem id v_table) then (*class, object and func cannot be assigned*)
						match (NameMap.find id v_table) with
						| Class(id) -> check_expr v_table c_table s_table env (NameMap.find id v_table) e2
						| FuncType(a, b) -> false
						| _ -> (check_expr v_table c_table s_table env (NameMap.find id v_table) e2)
					else
						raise (Failure("Cannot Find Identifier."))
				| BinaryOp(e1', op', e2') ->
					(
					match op' with
					| Dot ->
						(
						match (e1', e2') with
						| (Id(id1), Id(id2)) -> check_expr v_table c_table s_table env (find_cls_mem c_table id1 id2) e2
						| _ -> raise (Failure("Dot Operation Error"))
						) 
					| Index -> false(*?????????????*)
					)
			in check_left_type e1			
		| Trans -> false (*????????*)
		| At -> false (*???*)
		| Index -> false (*?????????*)
		| Dot -> 
			match (e1, e2) with
			| (Id(id1), Id(id2)) -> check_expr v_table c_table s_table env (find_cls_mem c_table id1 id2) e2
			| _ -> raise (Failure("Dot Operation Error"))
		)
	| FuncCall(e1, expr_list) -> false(*???????????*)
	| NoExpr -> true



(*find if a class exists*)
(*
and cls_exist id c_table =
	match c_table with
	| [] -> false
	| head::tail ->
		match head with
		| (cls_name, mem_list) ->
			if cls_name = id then
				true
			else
				cls_exist id tail
		| _ -> raise (Failure("Class Table Error"))
*)


(*find the member of the object in Class table*)

and find_cls_mem c_table id id' =
	if NameMap.mem id c_table then
		let rec find_mem list id =
			match list with
			| [] -> raise (Failure("Cannot Find Member "^id))
			| head::tail ->
				match head with
				| (m_id, m_type) -> 
					if m_id = id then
						m_type
					else
						find_mem tail id
				| _ -> raise (Failure("Class Table Error "))
		in find_mem (NameMap.find id c_table) id'
	else
		raise (Failure("Cannot Find Class "^id))
(*
and find_cls_mem c_table id id'=  (*id.id'*)
	match c_table with
	| [] -> raise (Failure("Cannot Find Class " ^ id))
	| head::tail ->
		(
		match head with
		| (cls_name, mem_list) ->
			if cls_name = id then
				let rec find_mem list id = 
					match list with
					| [] -> raise (Failure("Cannot Find Class Member "^id))
					| head::tail ->
						match head with
						| (m_id, m_type) ->
							if m_id = id then
								m_type
							else
								find_mem tail id
					| _ -> raise (Failure("Class Table Error 2."))
				in find_mem mem_list id'
			else
				find_cls_mem tail id id'
		| _ -> raise (Failure("Class Table Error 2."))
		)
	| _ -> raise (Failure("Class Table Error 1."))
*)
;;