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
(*2: Object*)
(*3: Loop*)

let rec check_semantic program =	 
	match program with
	| Program(stmt_list) -> List.fold_left (check_stmt 0(*outer env*)) (NameMap.empty, [], NameMap.empty) stmt_list

(*check statements*)

and check_stmt env (v_table, c_table, o_table) stmt = 
	match stmt with (*match all types of statements*)
	| BasicDecl(type_spec, basic_init_decl) -> 
		let rec check_basic_init_decl v_table list = (*recursively check the basic declaration list*)
			match list with
			| [] -> (v_table, c_table, o_table)
			| head::tail -> 
  			match head with
  			| BasicInitDefault(id) -> check_basic_init_decl (NameMap.add id type_spec v_table) tail
  			| BasicInitAssign(id, expr) ->
  				if(check_expr v_table c_table o_table env type_spec expr) then
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
		if check_expr v_table c_table o_table env Void expr then
			(v_table, c_table, o_table)
		else
			raise (Failure("Statement Error"))
	| _ -> raise (Failure("Not Finished"))

(*check expression*)

and check_expr v_table c_table o_table env type_spec expr = 
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
	| This ->
		if env = 2 then
			true
		else
			false
	| UnaryOp(op, expr) ->
		( 
		match op with
		| Plus -> check_expr v_table c_table o_table env Double expr
		| Minus -> check_expr v_table c_table o_table env Double expr
		| Not -> check_expr v_table c_table o_table env Bool expr
		)
	| BinaryOp(e1, op, e2) ->
		( 
		match op with
		| Plus -> (check_expr v_table c_table o_table env Double e1) && (check_expr v_table c_table o_table env Double e2)
		| Minus -> (check_expr v_table c_table o_table env Double e1) && (check_expr v_table c_table o_table env Double e2)
		| Mult -> (check_expr v_table c_table o_table env Double e1) && (check_expr v_table c_table o_table env Double e2)
		| Div -> (check_expr v_table c_table o_table env Double e1) && (check_expr v_table c_table o_table env Double e2)
		| Mod -> (check_expr v_table c_table o_table env Int e1) && (check_expr v_table c_table o_table env Int e2)
		| And -> (check_expr v_table c_table o_table env Bool e1) && (check_expr v_table c_table o_table env Bool e2)
		| Or -> (check_expr v_table c_table o_table env Bool e1) && (check_expr v_table c_table o_table env Bool e2)
		| Gt -> (check_expr v_table c_table o_table env Double e1) && (check_expr v_table c_table o_table env Double e2)
		| Ge -> (check_expr v_table c_table o_table env Double e1) && (check_expr v_table c_table o_table env Double e2)
		| Eq -> (check_expr v_table c_table o_table env Double e1) && (check_expr v_table c_table o_table env Double e2)
		| Neq -> (check_expr v_table c_table o_table env Double e1) && (check_expr v_table c_table o_table env Double e2)
		| Le -> (check_expr v_table c_table o_table env Double e1) && (check_expr v_table c_table o_table env Double e2)
		| Lt -> (check_expr v_table c_table o_table env Double e1) && (check_expr v_table c_table o_table env Double e2)
		| Assign -> 
			let check_left_type el =
				(*only when the left is an identifier or an A.B form can the assignment success*)
				match el with
				| Id(id) -> 
					if (NameMap.mem id v_table) then (*class, object and func cannot be assigned*)
						match (NameMap.find id v_table) with
						| Class(a) -> false
						| Object -> false
						| FuncType(a, b) -> false
						| _ -> (check_expr v_table c_table o_table env (NameMap.find id v_table) e2)
					else
						raise (Failure("Cannot Find Identifier."))
				| BinaryOp(e1', op', e2') ->
					match op' with
					| Dot ->
						match e1' with
						| Id(id) ->
							if (NameMap.mem id v_table) then
								if (NameMap.find id v_table = Object) then
									match e2' with
									| Id(id') ->
										find_obj_mem c_table (NameMap.find id o_table) id'
									| _ -> raise (Failure("Trans Operation Error."))
								else
									raise (Failure(id^" is not an object"))
							else
								raise (Failure("Cannot Find Identifier ")^id)
					| _ -> raise (Failure("Cannot Assign2."))
				| _ -> raise (Failure("Cannot Assign."))
			in check_left_type e1
		| Trans -> false (*?????*)
		| At -> false (*???*)
		| Dot -> 
			(
			match e1 with
			| Id(id) -> 
				if (NameMap.mem id v_table) then
					if (NameMap.find id v_table = Object) then
						match e2 with
						| Id(id') ->
							find_obj_mem c_table (NameMap.find id o_table) id'
						| _ -> raise (Failure("Trans Operation Error."))
					else
						raise (Failure(id^" is not an object"))
				else
					raise (Failure("Cannot Find Identifier "^id))
			)
		)
	| FuncCall(e1, expr_list) -> false(*???????????*)
	| NoExpr -> true

(*find the member of the object in Class table*)

and find_obj_mem list id id'=  (*id.id'*)
	match list with
	| [] -> raise (Failure("Cannot Find Class."))
	| head::tail ->
		match head with
		| (cls_name, mem_list) ->
			if cls_name = id then
				let rec find_cls_mem list id = 
					match list with
					| [] -> raise (Failure("Cannot Find Class Member "^id))
					| head::tail ->
						match head with
						| (m_type, m_id, m_arg) ->
							if m_id = id then
								match m_type with
								| Class(a) -> false
								| Object -> false
								| FuncType(a, b) -> false
								| _ -> (check_expr v_table c_table o_table env m_type e2)
							else
								find_cls_mem tail id
					| _ -> raise (Failure("Class Table Error 2."))
					in find_cls_mem mem_list id'
					else
						find_obj_mem tail id id'
	| _ -> raise (Failure("Class Table Error."))
;;
