

module PlcInterp

open Absyn
open Environ

let rec eval (e : expr) (env : plcVal env) : plcVal =
    match e with
    | ConI i -> IntV i
    | ConB b -> BoolV b
    | ESeq _ -> SeqV []
    | Var x  -> lookup env x

    | Prim1 (op, e1) -> let v1 = eval e1 env
                        match (op, v1) with
                        | ("hd", SeqV v) -> match v with
                                            | [] -> failwith "Prim1: sequence is empty"
                                            | h::t ->  h 
                        | ("tl", SeqV v) -> match v with
                                            | [] -> failwith "Prim1: sequence is empty"
                                            | h::t ->  SeqV t
                        | ("-", IntV i) -> IntV (- i)
                        | ("!", BoolV b) -> BoolV (not b)   
                        | ("ise", SeqV v) -> if (v = []) then (BoolV true) else (BoolV false)
                        | ("print", v) -> printf "%s" (val2string v) 
                                          ListV []      
                        | _ -> failwith "Prim1: Impossible"                    

    | Prim2 (op, e1, e2) -> let v1 = eval e1 env in
                              let v2 = eval e2 env in
                                match (op, v1, v2) with
                                | (";", _, _) -> v2                
                                | ("=", _, _) -> BoolV (v1 = v2)
                                | ("!=", _, _) -> BoolV (v1 <> v2)
                                | ("::", _, SeqV v) -> SeqV (List.append [v1] v) 
                                | ("&&", BoolV i1, BoolV i2) -> BoolV (i1 && i2)
                                | ("<", IntV i1, IntV i2) -> BoolV (i1 < i2)
                                | ("<=", IntV i1, IntV i2) -> BoolV (i1 <= i2)
                                | ("/", IntV i1, IntV i2) -> IntV (i1 / i2)            
                                | ("*", IntV i1, IntV i2) -> IntV (i1 * i2)
                                | ("+", IntV i1, IntV i2) -> IntV (i1 + i2)
                                | ("-", IntV i1, IntV i2) -> IntV (i1 - i2)
                                | _   -> failwith "Prim2: Impossible"

    | Let (x, e1, e2) -> let v = eval e1 env in
                           let letBodyEnv = (x, v) :: env in
                             eval e2 letBodyEnv

    | If (e1, e2, e3) -> let v1 = eval e1 env in
                           match v1 with
                           | BoolV true  -> eval e2 env
                           | BoolV false -> eval e3 env
                           | _ -> failwith "If: Impossible"

    | Letrec (f, _, x, _, e1, e2) -> let recBodyEnv = (f, Clos(f, x, e1, env)) :: env in
                                       eval e2 recBodyEnv

    | Anon (t, s, e1) -> Clos ("", s, e1, env)

    | Match (e1, elist) -> match (eval e1 env), elist with
                           | v1, [] -> failwith "Match: no match found"
                           | v1, (Some (eo), e1) :: t -> if (v1 = (eval eo env)) then (eval e1 env) else (eval (Match (e1, t)) env)
                           | v1, (None, e1) :: t -> eval e1 env

    | Call (f, e1) -> match f with
                      | Var v -> let v1 = lookup env v in
                                   match v1 with
                                   | Clos (f, x, ex, fEnv) -> let v = eval e1 env in
                                                                let closEnv = (x, v) :: (f, v1) :: fEnv in
                                                                  eval ex closEnv
                                   | _ -> failwith "Call: inner eval var match wrong type"
                      | Call (_, _) -> let v1 = eval f env in
                                         match v1 with
                                         | Clos (f, x, ex, fEnv) -> let v = eval e1 env in
                                                                      let closEnv = (x, v) :: (f, v1) :: fEnv in
                                                                        eval ex closEnv
                                         | _ -> failwith "Call: inner eval call match wrong type"
                      | _ -> failwith "Call: not first-order function"

    | List es -> ListV (List.map (fun e -> eval e env) es)

    | Item (n, e1) -> match eval e1 env with
                      | ListV vs -> List.item (n - 1) vs
                      | _ -> failwith "Item: Impossible"
