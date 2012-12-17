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
(*3: compond statement*)
(*4 state*)

let rec check_semantic program =	 
	match program with
	| Program(stmt_list) -> List.fold_left (check_stmt 0 0) (NameMap.empty, NameMap.empty, NameMap.empty) stmt_list

(*check statements*)

and check_stmt env level (v_table, c_table, s_table) stmt = 
	match stmt with (*match all types of statements*)
	| BasicDecl(type_spec, basic_init_decl) -> 
		let rec check_basic_init_decl v_table list = (*recursively check the basic declaration list*)
			match list with
			| [] -> (v_table, c_table, s_table)
			| head::tail -> 
  			match head with
  			| BasicInitDefault(id) -> check_basic_init_decl (NameMap.add id (type_spec, level) v_table) tail
  			| BasicInitAssign(id, expr) ->
  				if(check_expr v_table c_table s_table env (type_spec, level) expr) then
  					check_basic_init_decl 
						(
							if NameMap.mem id v_table then
								v_table
							else
								NameMap.add id (type_spec, level) v_table
						) 
						tail
  				else
  					raise (Failure("Basic Assignment Check Fails\n"))
		in check_basic_init_decl v_table basic_init_decl
	| ClassDecl(id, state_list, stmt_list) -> (*I am working on it!!!!!!!!!!!!!!!!!!*)
		if env = 0 then
			(v_table, (add_c_table v_table c_table s_table id stmt_list level), (add_s_table v_table c_table s_table id state_list level))
		else
			raise (Failure("Cannot Define Class"))					 
	| Expr(expr) -> 
		if check_expr v_table c_table s_table env (Void, level) expr then
			(v_table, c_table, s_table)
		else
			raise (Failure("Statement Error"))
	| _ -> raise (Failure("Not Finished"))

(*check expression*)

and check_expr v_table c_table s_table env (type_spec, level) expr = 
	match expr with (*match all types of expressions*)
	| Id(id) -> 
		if NameMap.mem id v_table then
			match (type_spec) with
			| Int -> 
				(
				match NameMap.find id v_table with
				| (Int, _) -> true
				| (Char, _) -> true
				| _ -> false
				)
			| Double -> 
				match NameMap.find id v_table with
				| (Int, _) -> true
				| (Char, _) -> true
				| (Double, _) -> true
				| _ -> false
			| els -> 
				match NameMap.find id v_table with
				| (some_type, _) -> type_spec = some_type
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
		if env = 4 then
			true
		else
			false
	| UnaryOp(op, expr) ->
		( 
		match op with
		| Plus -> check_expr v_table c_table s_table env (Double, level) expr
		| Minus -> check_expr v_table c_table s_table env (Double, level) expr
		| Not -> check_expr v_table c_table s_table env (Bool, level) expr
		)
	| BinaryOp(e1, op, e2) ->
		( 
		match op with
		| Plus -> (check_expr v_table c_table s_table env (Double, level) e1) && (check_expr v_table c_table s_table env (Double, level) e2)
		| Minus -> (check_expr v_table c_table s_table env (Double, level) e1) && (check_expr v_table c_table s_table env (Double, level) e2)
		| Mult -> (check_expr v_table c_table s_table env (Double, level) e1) && (check_expr v_table c_table s_table env (Double, level) e2)
		| Div -> (check_expr v_table c_table s_table env (Double, level) e1) && (check_expr v_table c_table s_table env (Double, level) e2)
		| Mod -> (check_expr v_table c_table s_table env (Int, level) e1) && (check_expr v_table c_table s_table env (Int, level) e2)
		| And -> (check_expr v_table c_table s_table env (Bool, level) e1) && (check_expr v_table c_table s_table env (Bool, level) e2)
		| Or -> (check_expr v_table c_table s_table env (Bool, level) e1) && (check_expr v_table c_table s_table env (Bool, level) e2)
		| Gt -> (check_expr v_table c_table s_table env (Double, level) e1) && (check_expr v_table c_table s_table env (Double, level) e2)
		| Ge -> (check_expr v_table c_table s_table env (Double, level) e1) && (check_expr v_table c_table s_table env (Double, level) e2)
		| Eq -> (check_expr v_table c_table s_table env (Double, level) e1) && (check_expr v_table c_table s_table env (Double, level) e2)
		| Neq -> (check_expr v_table c_table s_table env (Double, level) e1) && (check_expr v_table c_table s_table env (Double, level) e2)
		| Le -> (check_expr v_table c_table s_table env (Double, level) e1) && (check_expr v_table c_table s_table env (Double, level) e2)
		| Lt -> (check_expr v_table c_table s_table env (Double, level) e1) && (check_expr v_table c_table s_table env (Double, level) e2)
		| Assign -> 
			let check_left_type el =
				(*only when the left is an identifier, A.B or A:[B] can the assignment success*)
				match el with
				| Id(id) -> 
					if (NameMap.mem id v_table) then (*class, object and func cannot be assigned*)
						match (NameMap.find id v_table) with
						| (Class(id), _) -> check_expr v_table c_table s_table env (NameMap.find id v_table) e2
						| (FuncType(a, b), _) -> false
						| _ -> (check_expr v_table c_table s_table env (NameMap.find id v_table) e2)
					else
						raise (Failure("Cannot Find Identifier."))
				| BinaryOp(e1', op', e2') ->
					(
					match op' with
					| Dot ->
						(
						match (e1', e2') with
						| (Id(id1), Id(id2)) -> check_expr v_table c_table s_table env ((find_cls_mem c_table id1 id2), level) e2
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
			| (Id(id1), Id(id2)) -> check_expr v_table c_table s_table env ((find_cls_mem c_table id1 id2), level) e2
			| _ -> raise (Failure("Dot Operation Error"))
		)
	| FuncCall(e1, expr_list) -> false(*???????????*)
	| NoExpr -> true

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

(*add states to the state table*)

and add_s_table v_table c_table s_table id state_list level = 
	let rec add_state s_table list = 
		match list with
		| [] -> s_table
		| head::tail ->
			match head with
			| (s_id, c_stmt) -> 
				match c_stmt with
				| CompStmt(t) -> 
					match check_stmt 4 (level + 1) (v_table, c_table, s_table) c_stmt with
					| (_,_,_) -> 
						add_state 
						(
						if NameMap.mem id s_table then
							NameMap.add id (s_id::(NameMap.find id s_table)) s_table
						else
							NameMap.add id [s_id] s_table
						) 
						tail
					| _ -> raise (Failure("Statement Error"))
				| _ -> raise (Failure("Need an cstmt"))
			| _ -> raise (Failure("Add State Error")) 
	in add_state s_table state_list

(*add class to the class table*)

and add_c_table v_table c_table s_table id stmt_list level =
	let rec add_class c_table list =
		match list with
		| [] -> c_table
		| head::tail ->
			match head with
			| BasicDecl(type_spec, basic_init_decl_list) ->
				(
				match check_stmt 1 (level + 1) (v_table, c_table, s_table) head with
				| (v_table',_,_) -> 
					let rec add_to_table c_table list1 list2 = 
						match (list1, list2) with
						| ([] ,[]) -> c_table
						| (hd1::tl1, hd2::tl2) -> 
							if hd1 = hd2 then
								add_to_table c_table tl1 tl2 
							else
								match (hd1, hd2) with
								| ((id1, (t1, l1)), (id2, (t2, l2))) ->
									if id1 = id2 then
										add_to_table 
										(
										if NameMap.mem id c_table then
											NameMap.add id ((id2, t2)::(NameMap.find id c_table)) c_table
										else
											NameMap.add id [(id2, t2)] c_table
										) 
										tl1 tl2
									else
										add_to_table 
										(
										if NameMap.mem id c_table then
											NameMap.add id ((id2, t2)::(NameMap.find id c_table)) c_table
										else
											NameMap.add id [(id2, t2)] c_table
										) 
										list1 tl2
								| _ -> raise(Failure("Error")) 
					in add_to_table c_table (NameMap.bindings v_table) (NameMap.bindings v_table')
				| _ -> raise(Failure("Statement Error"))
				)
			| FuncDecl(arg1, arg2) ->
				(
				match check_stmt 1 (level + 1) (v_table, c_table, s_table) head with
				| (v_table',_,_) -> 
					let rec add_to_table c_table list1 list2 = 
						match (list1, list2) with
						| ([] ,[]) -> c_table
						| (hd1::tl1, hd2::tl2) -> 
							if hd1 = hd2 then
								add_to_table c_table tl1 tl2 
							else
								match (hd1, hd2) with
								| ((id1, (t1, l1)), (id2, (t2, l2))) ->
									if id1 = id2 then
										add_to_table 
										(
										if NameMap.mem id c_table then
											NameMap.add id ((id2, t2)::(NameMap.find id c_table)) c_table
										else
											NameMap.add id [(id2, t2)] c_table
										) 
										tl1 tl2
									else
										add_to_table 
										(
										if NameMap.mem id c_table then
											NameMap.add id ((id2, t2)::(NameMap.find id c_table)) c_table
										else
											NameMap.add id [(id2, t2)] c_table
										) 
										list1 tl2
								| _ -> raise(Failure("Error")) 
					in add_to_table c_table (NameMap.bindings v_table) (NameMap.bindings v_table')
				| _ -> raise(Failure("Statement Error"))
				)
			| _ -> c_table 
	in add_class c_table stmt_list
	 
;;