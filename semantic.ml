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
(*2: while Loop*)
(*3: compond statement*)
(*4: state*)
(*5: if*)
(*6: function*)
(*7: for loop*)

let rec check_semantic program =	 
	match program with
	| Program(stmt_list) -> List.fold_left (check_stmt 0 [(0,"")]) 
	(
	List.fold_right2
	(fun id t -> NameMap.add id t) 
	["printInt"; "printDouble"; "printChar"; "printBool"; "printStr"] 
	[(FuncType(Void,[Int]), [(0,"")]); (FuncType(Void,[Double]), [(0,"")]); (FuncType(Void,[Char]), [(0,"")]); (FuncType(Void,[Bool]), [(0,"")]); (FuncType(Void,[ListType(Char)]), [(0,"")])] 
	NameMap.empty, 
	NameMap.empty, NameMap.empty
	) 
	stmt_list

(*check statements*)

and check_stmt env level (v_table, c_table, s_table) stmt = 
	match stmt with (*match all types of statements*)
	| BasicDecl(type_spec, basic_init_decl) -> 
		let rec check_basic_init_decl v_table list = (*recursively check the basic declaration list*)
			match list with
			| [] -> (v_table, c_table, s_table)
			| head::tail -> 
  			match head with
  			| BasicInitDefault(id) -> 
					check_basic_init_decl (check_redefine id type_spec level v_table c_table env) tail
  			| BasicInitAssign(id, expr) ->
  				if (type_compatable type_spec (check_expr v_table c_table s_table env level expr)) then
  					check_basic_init_decl (check_redefine id type_spec level v_table c_table env) tail
  				else
  					raise (Failure("Basic Assignment Check Fails"))
		in check_basic_init_decl v_table basic_init_decl
	| FuncDecl(type_spec, id, expr) ->
		if (check_expr (check_redefine id type_spec level v_table c_table env) c_table s_table env ((6, id)::level) expr) = type_spec then	
			(check_redefine id type_spec level v_table c_table env, c_table, s_table)
		else
			raise(Failure("Function Type Mismatch"))
	| ClassDecl(id, state_list, stmt_list) ->
		ignore(check_redefine id Void level v_table c_table env);
		if env = 0 then
			match add_s_c_table v_table (NameMap.add id [] c_table) (NameMap.add id [] s_table) id state_list stmt_list ((1, id)::level) with
			| (c_table', s_table') -> (v_table, c_table', s_table')
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
				match check_stmt 3 
				(
					match level with
					| (6, _)::_ 
					| (7, _)::_ -> level
					| _ -> (3, "")::level
				) 
					(v_table', c_table, s_table) head with
				| (v_table'', c_table, s_table) -> check_comp_stmt v_table'' tail
		in check_comp_stmt v_table stmt_list
	| If(expr, stmt1, stmt2) ->
		ignore (check_stmt env ((5, "")::level) (v_table, c_table, s_table) stmt1);
		ignore (check_stmt env ((5, "")::level) (v_table, c_table, s_table) stmt2);
		if (check_expr v_table c_table s_table env level expr) = Bool then 
			(v_table, c_table, s_table)
		else 
			raise (Failure("If Statement Error"))
	| For(stmt1, expr1, expr2, stmt2) ->
		match stmt1 with
		| BasicDecl(_, _) | Expr(_) | NoStmt ->
			(
			match check_stmt env ((7, ""):: level) (v_table, c_table, s_table) stmt1 with
			| (v_table, c_table, s_table) ->
				if (check_expr v_table c_table s_table env level expr1) = Bool then
					match check_expr v_table c_table s_table env level expr1 with
					| _ -> check_stmt env ((7, ""):: level) (v_table, c_table, s_table) stmt2
				else
					raise(Failure("Expect a Bool Expr in For"))
			)
		| _ -> raise(Failure("Cannot Define Such Stmt in For")) 
	| While(expr, stmt1) ->
		ignore (check_stmt env ((2, "")::level) (v_table, c_table, s_table) stmt1);
		if (check_expr v_table c_table s_table env level expr) = Bool then 
			(v_table, c_table, s_table)
		else 
			raise (Failure("Expect a Bool Expr in While"))
	| DoWhile(stmt, expr) ->
		ignore (check_stmt env ((2, "")::level) (v_table, c_table, s_table) stmt);
		if (check_expr v_table c_table s_table env level expr) = Bool then 
			(v_table, c_table, s_table)
		else
			raise (Failure("Expect a Bool Expr in Dowhile"))
	| Continue ->
		if env = 2 || env = 7 then
			(v_table, c_table, s_table)
		else
			raise(Failure("Continue Must Be in Loop"))
	| Break ->
		if env = 2 || env = 7 then
			(v_table, c_table, s_table)
		else
			raise(Failure("Break Must Be in Loop"))
	| Return(expr) ->
		(
		let rec find_func_type list =
			match list with
			| [] -> raise(Failure("Unkown Error"))
			| head::tail -> 
				match head with
				| (environment, name) ->
					if environment = 6 then
						match NameMap.find name v_table with
						| (FuncType(func_type,_),_) -> 
							if func_type = Void then
								raise(Failure("Function Has No Return"))
							else
								if (check_expr v_table c_table s_table env level expr) = func_type then
									(v_table, c_table, s_table)
								else
									raise(Failure("Function Return Type Mismatch"))
						|	_ -> raise(Failure(name^" is not a Function Type"))
					else
						find_func_type tail
		in find_func_type level
		) 
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
		| ListLit(type_spec, expr_list) -> 
			let rec check_expr_list list =
				match list with
				| [] -> ListType(type_spec)
				| head::tail ->
					if type_compatable type_spec (check_expr v_table c_table s_table env level head) then
						check_expr_list tail
					else
						raise(Failure("List Element Type Mismatch"))
			in check_expr_list expr_list
		)
	| FuncLit(type_spec, param_list, stmt) ->
		let rec get_func_param type_spec_list list v_table' = 
			match list with
			| [] -> 
				ignore(check_stmt env level (v_table', c_table, s_table) stmt);
				FuncType(type_spec, List.rev type_spec_list)
			| head::tail -> 
				match head with
				| (t, name) ->  get_func_param (t::type_spec_list) tail (check_redefine name t level v_table' c_table env)
		in get_func_param [] param_list v_table
	| This ->
		if env = 4 then
			match level with
			| hd1::(hd2::tail) ->
				(
				match hd2 with
				| (_, id) -> Class(id)
				)
			| _ -> raise(Failure("Unknown Error")) 
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
					if (NameMap.mem id v_table) then
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
					| Index ->
						(
						match (e1', e2') with
						| (Id(id1), _) ->
							if NameMap.mem id1 v_table then
								match NameMap.find id1 v_table with
								| (ListType(t), _) ->
									if (check_expr v_table c_table s_table env level e2) = t then
										t
									else
										raise(Failure("List Type Mismatch"))
								| _ -> raise (Failure(id1^" is not a List"))
							else
								raise (Failure("Cannot Find List " ^ id1))
						| _ -> raise (Failure("Index Operation Error"))
						)	
					| _ -> raise(Failure("Assignment Fails"))
					)
				| _ -> raise(Failure("Assignment Error"))	
			in check_left_type e1			
		| Trans ->
			(
			match(e1, e2) with
			| (Id(id1), Id(id2)) -> 
				if find_cls_state s_table id1 id2 then
					(*
					match NameMap.find id1 v_table with
					| (type_spec, _) -> type_spec
					*)
					Void 
				else
					raise(Failure("Cannot Find State "^id2))
			| (This, Id(id2)) ->
				let rec helper list =
					match list with
					| (1, id1)::tail ->
						if find_cls_state s_table id1 id2 then
    					(*
    					match NameMap.find id1 v_table with
    					| (type_spec, _) -> type_spec
    					*)
    					Void 
    				else
    					raise(Failure("Cannot Find State "^id2))
					| hd::tail -> helper tail
				in helper level
			| _ -> raise(Failure("Trans Operation Error"))
			)
		| At ->
			(
			match(e1, e2) with
			| (Id(id1), Id(id2)) -> 
				if find_cls_state s_table id1 id2 then
					Bool 
				else
					raise(Failure("Cannot Find State "^id2))
			| _ -> raise(Failure("At Operation Error"))
			)
		| Index ->
			(
			match check_expr v_table c_table s_table env level e1 with
			| ListType(t) -> 
				if (check_expr v_table c_table s_table env level e2) = Int then
					ListType(t)
				else
					raise(Failure("Index Number Must Be an Integer"))
			| _ -> raise(Failure("Index Operation Error"))
			)
		| Dot -> 
			(
			match (e1, e2) with
			| (Id(id1), Id(id2)) -> find_cls_mem c_table id1 id2
			| _ -> raise(Failure("Dot Operation Error"))
			)
		| LDot -> 
			(
			match ((check_expr v_table c_table s_table env level e1), e2) with
			| (ListType(list_type), FuncCall(name, e_list)) ->
				(
				match (name, e_list) with
				| (Id("insert"), [e1; e2]) | (Id("append"), [e1; e2]) ->
					(
    			match ((check_expr v_table c_table s_table env level e1), (check_expr v_table c_table s_table env level e2)) with
    			| (Int, t) -> if t = list_type then Void else raise(Failure("Function Argument Type Mismatch"))
    			| _ -> raise(Failure("Function Argument Type Mismatch"))
					)
				|	(Id("remove"), [e1]) ->
					(
					match (check_expr v_table c_table s_table env level e1) with
					| Int -> list_type
					| _ -> raise(Failure("Function Argument Type Mismatch"))
					)
				| (Id("filter"), [e1]) ->
					(
					match (check_expr v_table c_table s_table env level e1) with
					| FuncType(return_type, arg_list) ->
						(
						match arg_list with
						| t::[] ->
							if t = list_type && Bool = return_type then ListType(list_type) else raise(Failure("Function Argument Type Mismatch"))
						| _ -> 	raise(Failure("Function Argument Mismatch"))
						)
					| _ -> raise(Failure("Function Argument Type Mismatch"))
					)
				| (Id("count"), [e1]) ->
					(
					match (check_expr v_table c_table s_table env level e1) with
					| FuncType(return_type, arg_list) ->
						(
						match arg_list with
						| t::[] ->
							if t = list_type && Bool = return_type then Int else raise(Failure("Function Argument Type Mismatch"))
						| _ -> 	raise(Failure("Function Argument Mismatch"))
						)
					| _ -> raise(Failure("Function Argument Type Mismatch"))
					)
				| _ -> raise(Failure("No Such Function"))
				)
			| _ -> raise(Failure("LDot Operation Error"))
			)
		)
	| FuncCall(e1, expr_list) ->
		(
		match e1 with
		| Id(id) ->
			if NameMap.mem id v_table then
				match NameMap.find id v_table with
				| (FuncType(type_spec, type_list),_) ->
					let rec check_param type_list expr_list =
						 match (type_list, expr_list) with
						| ([], []) -> type_spec
						| (t::tail1, e::tail2) -> 
							if (check_expr v_table c_table s_table env level e) = t then
								check_param tail1 tail2
							else
								raise(Failure("Function Parameter Type Mismatch"))
						| _ -> raise(Failure("Function Parameter Mismatch"))
					in check_param type_list expr_list
				| _ -> raise(Failure(id^" is not an Function"))
			else
				raise(Failure("Cannot Find Function "^id))
		| _ -> raise(Failure("Function Call Format Error"))
		)
	| NoExpr -> Void

(*find the state of the object in state table*)

and find_cls_state s_table id id' = 
	if NameMap.mem id s_table then
		let rec find_state list id = 
			match list with
			| [] -> raise(Failure("Cannot Find State "^id))
			| head::tail ->
				if head = id then
					true
				else
					false
		in find_state (NameMap.find id s_table) id'
	else
		raise(Failure("Cannot Find Class "^id))

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
		in find_mem (NameMap.find id c_table) id'
	else
		raise (Failure("Cannot Find Class "^id))
		
(*add states and stmts to the table*)

and add_s_c_table v_table c_table s_table id state_list stmt_list level =
	match add_c_table v_table c_table s_table id stmt_list level with
	| (v_table', c_table') ->
		match add_s_table v_table' c_table s_table id state_list level with
		| (s_table') -> 		
  		if check_state v_table' c_table' s_table' id state_list level then 
  			(c_table', s_table')
  		else
  			raise(Failure("States Error"))

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
					(
					match check_stmt 4 ((4, "")::level) (v_table, c_table, s_table) c_stmt with
					| (_,_,_) -> check_each_state tail
					)
				| _ -> raise (Failure("Need a Compound Stmt")) 
	in check_each_state state_list

(*add states to the state table*)

and add_s_table v_table c_table s_table id state_list level = 
	let rec add_state s_table list = 
		match list with
		| [] ->
			if NameMap.mem id s_table then 
				s_table
			else
				NameMap.add id [] s_table
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
				| _ -> raise (Failure("Need a Compound Stmt")) 
	in add_state s_table state_list

(*add class to the class table*)

and add_c_table v_table c_table s_table id stmt_list level =
	let rec add_class v_table c_table list =
		match list with
		| [] -> 
			(v_table, 
			if NameMap.mem id c_table then
				c_table
			else
				NameMap.add id [] c_table
			)
		| head::tail ->
			match head with
			| BasicDecl(_, _) | FuncDecl(_, _, _) ->
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
								(
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
								)
						| _ -> raise(Failure("Unknown Error"))		
					in add_to_table c_table (NameMap.bindings v_table) (NameMap.bindings v_table')
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

(*check whether there is a name conflict*)

and check_redefine id type_spec level v_table c_table env = 
	if NameMap.mem id v_table then
		match NameMap.find id v_table with
		| (_, level') ->
			if level = level' then
				raise(Failure("Name Conflict"))
			else
				if NameMap.mem id c_table && env = 0 then
					raise(Failure("Name Conflict"))
				else
					NameMap.add id (type_spec, level) v_table
	else
		if NameMap.mem id c_table && env = 0 then
			raise(Failure("Name Conflict"))
		else
			NameMap.add id (type_spec, level) v_table
;;