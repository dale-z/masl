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
(*4: state*)
(*5: if*)

let rec check_semantic program =	 
	match program with
	| Program(stmt_list) -> List.fold_left (check_stmt 0 [(0,"")]) (NameMap.empty, NameMap.empty, NameMap.empty) stmt_list

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
  				if (type_compatable type_spec (check_expr v_table c_table s_table env level expr)) then
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
			match add_s_c_table v_table c_table s_table id state_list stmt_list ((1, id)::level) with
			| (c_table, s_table) -> (v_table, c_table, s_table)
			(*(v_table, (add_c_table v_table c_table s_table id stmt_list level), (add_s_table v_table c_table s_table id state_list level))*)
		else
			raise (Failure("Cannot Define Class"))					 
	| Expr(expr) -> 
		ignore (check_expr v_table c_table s_table env level expr);
		(v_table, c_table, s_table)
	| CompStmt(stmt_list) -> 
		let rec check_comp_stmt v_table' list = 
			match list with
			| [] -> (v_table, c_table, s_table)
			| head::tail -> 
				match check_stmt 3 ((3, "")::level) (v_table', c_table, s_table) head with
				| (v_table'', c_table, s_table) -> check_comp_stmt v_table'' tail
		in check_comp_stmt v_table stmt_list
	| If(expr, stmt1, stmt2) ->
		ignore (check_stmt env ((5, "")::level) (v_table, c_table, s_table) stmt1);
		ignore (check_stmt env ((5, "")::level) (v_table, c_table, s_table) stmt2);
		if (check_expr v_table c_table s_table env level expr) = Bool then 
			(v_table, c_table, s_table)
		else 
			raise (Failure("If Statement Error"))
	| NoStmt -> (v_table, c_table, s_table)
	| _ -> raise (Failure("Not Finished"))

(*check expression*)

and check_expr v_table c_table s_table env level expr = 
	match expr with (*match all types of expressions*)
	| Id(id) -> 
		if NameMap.mem id v_table then
			match NameMap.find id v_table with
			| (some_type, _) -> some_type
		else
			raise (Failure("Cannot Find Identifier " ^ id))
	| BasicLit(basic_literal) ->
		(
		match basic_literal with
		| IntLit(t) -> Int
		| DoubleLit(t) -> Double
		| CharLit(t) ->  Char
		| BoolLit(t) -> Bool
		| ObjectLit(object_literal) -> object_literal
		| ListLit(type_spec, expr_list) -> ListType(type_spec)
		)
	| FuncLit(type_spec, param_list, stmt) -> 
		let rec get_func_param type_spec_list list = 
			match list with
			| [] -> FuncType(type_spec, List.rev type_spec_list)
			| head::tail -> 
				match head with
				| (t, _) ->  get_func_param (t::type_spec_list) tail
		in get_func_param [] param_list
	| This ->
		if env = 4 then
			match level with
			| hd1::(hd2::tail) ->
				match hd2 with
				| (_, id) -> Class(id) 
		else
			raise(Failure("Cannot Use This Operator Heres"))
	| UnaryOp(op, expr) ->
		( 
		match op with
		| Plus 
		| Minus ->
			( 
			let helper t =
				judge_alg_type t t
			in helper (check_expr v_table c_table s_table env level expr)
			)
		| Not -> 
			match check_expr v_table c_table s_table env level expr with
			| Bool -> Bool
			| _ -> raise(Failure("Type Mismatch"))
		)
	| BinaryOp(e1, op, e2) ->
		( 
		match op with
		| Plus | Minus | Mult
		| Div -> judge_alg_type (check_expr v_table c_table s_table env level e1) (check_expr v_table c_table s_table env level e2)
		| Mod ->
			(
			match ((check_expr v_table c_table s_table env level e1), (check_expr v_table c_table s_table env level e2)) with
			| (Int, Int)
			| (Int, Char)
			| (Char, Int) -> Int
			| _ -> raise(Failure("Type Mismatch"))
			)
		| And 
		| Or ->
			(
			match ((check_expr v_table c_table s_table env level e1), (check_expr v_table c_table s_table env level e2)) with
			| (Bool, Bool) -> Int
			| _ -> raise(Failure("Type Mismatch"))
			)
		| Gt | Ge | Eq | Neq | Le
		| Lt -> 
			judge_logic_type (check_expr v_table c_table s_table env level e1) (check_expr v_table c_table s_table env level e2)
		| Assign -> 
			let check_left_type el =
				(*only when the left is an identifier, A.B or A:[B] can the assignment success*)
				match el with
				| Id(id) -> 
					if (NameMap.mem id v_table) then (*class, object and func cannot be assigned*)
						match (NameMap.find id v_table) with
						| (Class(name), _) ->
							if (check_expr v_table c_table s_table env level e2) = Class(name) then
								Class(name)
							else
								raise(Failure("Assignment Fails"))	
						| (FuncType(arg1, arg2), _) -> 
							if (check_expr v_table c_table s_table env level e2) = FuncType(arg1, arg2) then
								FuncType(arg1, arg2)
							else
								 raise(Failure("Assignment Fails"))
						| (ListType(arg), _) ->
							if (check_expr v_table c_table s_table env level e2) = ListType(arg) then
								ListType(arg)
							else
								raise(Failure("Assignment Fails"))
						| (type_spec, _) -> 
							match (type_spec, (check_expr v_table c_table s_table env level e2)) with
							| (Double, Int) | (Double, Char) | (Double, Double) -> Double
							| (Int, Char) | (Int, Int) -> Int
							| (Char, Char) -> Char
							| (Bool, Bool) -> Bool
							| _ -> raise(Failure("Assignment Fails"))
					else
						raise (Failure("Cannot Find Identifier "^id))
				| BinaryOp(e1', op', e2') ->
					(
					match op' with
					| Dot -> 
						(
						match (e1', e2') with
						| (Id(id1), Id(id2)) -> 
							if (find_cls_mem c_table id1 id2) = check_expr v_table c_table s_table env level e2 then
								(find_cls_mem c_table id1 id2)
							else
								raise(Failure("Cannot Find Class Member"))
						| _ -> raise (Failure("Dot Operation Error"))
						) 
					| Index -> Void(*?????????????*)
					| _ -> raise(Failure("Assignment Fails"))
					)
			in check_left_type e1			
		| Trans -> Void (*????????*)
		| At -> Void (*???*)
		| Index -> Void (*?????????*)
		| Dot -> Void (*??????????????*)
		| LDot -> Void(*???????????*)
		)
	| FuncCall(e1, expr_list) -> Void(*???????????*)
	| NoExpr -> Void

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
		
(*add states and stmts to the table*)

and add_s_c_table v_table c_table s_table id state_list stmt_list level =
	match add_c_table v_table c_table s_table id stmt_list level with
	| (v_table', c_table') ->
		if check_state v_table' c_table' s_table id state_list level then 
			(c_table', add_s_table v_table' c_table s_table id state_list level)
		else
			raise(Failure("States Error"))
	| _ -> raise (Failure("Fatal Error"))

(*check states of the class*)

and check_state v_table c_table s_table id state_list level =
	let rec check_each_state list = 
		match list with
		| [] -> true
		| head::tail ->
			match head with
			| (s_id, c_stmt) -> 
				match c_stmt with
				| CompStmt(t) -> 
					match check_stmt 4 ((4, "")::level) (v_table, c_table, s_table) c_stmt with
					| (_,_,_) -> check_each_state tail
					| _ -> raise (Failure("Statement Error"))
				| _ -> raise (Failure("Need an cstmt"))
			| _ -> raise (Failure("Add State Error")) 
	in check_each_state state_list

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
					add_state 
					(
					if NameMap.mem id s_table then
						NameMap.add id (s_id::(NameMap.find id s_table)) s_table
					else
						NameMap.add id [s_id] s_table
					) 
					tail
				| _ -> raise (Failure("Need an Compound Stmt"))
			| _ -> raise (Failure("Add State Error")) 
	in add_state s_table state_list

(*add class to the class table*)

and add_c_table v_table c_table s_table id stmt_list level =
	let rec add_class v_table c_table list =
		match list with
		| [] -> (v_table, c_table)
		| head::tail ->
			match head with
			| BasicDecl(type_spec, basic_init_decl_list) ->
				(
				match check_stmt 1 (level) (v_table, c_table, s_table) head with
				| (v_table',_,_) -> 
					let rec add_to_table c_table list1 list2 = 
						match (list1, list2) with
						| ([] ,[]) -> add_class v_table' c_table tail
						| ([], (id2, (t2, l2))::tl2) ->
							add_to_table
							(
							if NameMap.mem id c_table then
								NameMap.add id ((id2, t2)::(NameMap.find id c_table)) c_table
							else
								NameMap.add id [(id2, t2)] c_table
							)
							list1 tl2
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
			| FuncDecl(arg1, arg2, arg3) ->
				(
				match check_stmt 1 (level) (v_table, c_table, s_table) head with
				| (v_table',_,_) -> 
					let rec add_to_table c_table list1 list2 = 
						match (list1, list2) with
						| ([] ,[]) -> add_class v_table' c_table tail
						| ([], (id2, (t2, l2))::tl2) ->
							add_to_table
							(
							if NameMap.mem id c_table then
								NameMap.add id ((id2, t2)::(NameMap.find id c_table)) c_table
							else
								NameMap.add id [(id2, t2)] c_table
							)
							list1 tl2
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
			| _ -> add_class v_table c_table tail 
	in add_class v_table c_table stmt_list
	
(*judge what type should be returned*)

and judge_alg_type t1 t2 = 
	match (t1, t2) with
	| (Int, Int) -> Int
	| (Double, Double) -> Double
	| (Char, Char) -> Char
	| (Int, Double) -> Double
	| (Int, Char) -> Int
	| (Double, Int) -> Double
	| (Double, Char) -> Double
	| (Char, Int) -> Char
	| (Char, Double) -> Double
	| _ -> raise(Failure("Type Mismatch")) 	 

and judge_logic_type t1 t2 = 
	match (t1, t2) with
	| (Int, Int)
	| (Double, Double)
	| (Char, Char)
	| (Int, Double)
	| (Int, Char)
	| (Double, Int)
	| (Double, Char)
	| (Char, Int)
	| (Char, Double) -> Bool
	| _ -> raise(Failure("Type Mismatch"))

and type_compatable left right = 
	match (left, right) with
	| (Char, Char)
	| (Int, Char) | (Int, Int)
	| (Double, Char) | (Double, Int) | (Double, Double) -> true
	| (a, b) -> a = b
;;