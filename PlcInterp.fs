

module PlcInterp

open Absyn
open Environ

let rec eval (e : expr) (env : plcVal env) : plcVal =
    match e with
    | ConI i -> IntV i
    | ConB b -> BoolV b
    | ESeq _ -> SeqV []

    | Var x  ->
      let v = lookup env x in
      match v with
      | IntV  _ -> v
      | BoolV _ -> v
      | ListV  _ -> v
      | SeqV _ -> v
      | _      -> failwith ("Value of variable _" + x + "_ is not first-order.")

    | Prim1 (op, e1) ->
      match (op, e1) with
      | ("hd", List s) -> failwith "implement me"             // need to implement and test
      | ("tl", List s) -> failwith "implement me"             // full tail wanted not tail element
      | (_, _) -> let v1 = eval e1 env in
                    match (op, v1) with
                    | ("-", IntV i) -> IntV (- i)
                    | ("!", BoolV b) -> BoolV (not b)         // fix and test ise below (might need to be Seq)
                    | ("ise", v) -> if (v = ListV []) then (BoolV true) else (BoolV false)
                    | ("print", v) -> printf "%s" (val2string v) 
                                      ListV []      
                    | _ -> failwith "Impossible"

    | Prim2 (op, e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match (op, v1, v2) with
      | (";", _, _) -> v2                
      | ("=", _, _) -> BoolV (v1 = v2)
      | ("!=", _, _) -> BoolV (v1 <> v2)
      | ("::", _, ListV i2) -> ListV (v1 :: i2)               // need to fix/test (might be Seq type not list)
      | ("&&", BoolV i1, BoolV i2) -> BoolV (i1 && i2)
      | ("<", IntV i1, IntV i2) -> BoolV (i1 < i2)
      | ("<=", IntV i1, IntV i2) -> BoolV (i1 <= i2)
      | ("/", IntV i1, IntV i2) -> IntV (i1 / i2)            
      | ("*", IntV i1, IntV i2) -> IntV (i1 * i2)
      | ("+", IntV i1, IntV i2) -> IntV (i1 + i2)
      | ("-", IntV i1, IntV i2) -> IntV (i1 - i2)
      | _   -> failwith "Impossible"

    | Let (x, e1, e2) ->
      let v = eval e1 env in
      let env2 = (x, v) :: env in
      eval e2 env2

    | If (e1, e2, e3) ->
      let v1 = eval e1 env in
      match v1 with
      | BoolV true  -> eval e2 env
      | BoolV false -> eval e3 env
      | _ -> failwith "Impossible"

    | Letrec (f, _, x, _, e1, e2) ->                    
      let env2 = (f, Clos(f, x, e1, env)) :: env in
      eval e2 env2

    | Anon (t, s, e1) -> let sEnv = (s, lookup env s) :: env
                         eval e1 sEnv

    | Match (e1, elist) -> match (eval e1 env), elist with
                           | v1, [] -> failwith "Match: no match found"
                           | v1, (Some (eo), e1) :: t -> if (v1 = (eval eo env)) then (eval e1 env) else (eval (Match (e1, t)) env)
                           | v1, (None, e1) :: t -> eval e1 env

    | Call (Var f, e1) ->
      let c = lookup env f in
      match c with
      | Clos (f, x, e2, fenv) ->
        let v = eval e1 env in
        let env1 = (x, v) :: (f, c) :: fenv in
        eval e2 env1
      | _ -> failwith "eval Call: not a function"
    | Call _ -> failwith "eval Call: not first-order function"

    | List es -> ListV (List.map (fun e -> eval e env) es)

    | Item (n, e1) ->
      match eval e1 env with
      | ListV vs -> List.item (n - 1) vs
      | _ -> failwith "Impossible"