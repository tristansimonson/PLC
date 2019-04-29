

module PlcInterp

open Absyn
open Environ

let rec findMatch (v : plcVal) (elist: (expr option * expr) list) : expr =
  match v, elist with
  | v1, [] -> failwith "Match: no match found"
  | v1, (eo, e1) :: t -> if (v1 = eo || eo = "_") then e1 else (findMatch v t)

let rec eval (e : expr) (env : plcVal env) : plcVal =
    match e with
    | ConI i -> IntV i
    | ConB b -> BoolV b
    | ESeq s -> match s with                                       // definitely wrong
                | h :: [] -> SeqT eval h env
                | h :: t -> SeqT List.append [eval h env] [eval t env]

    | Var x  ->
      let v = lookup env x in
      match v with
      | IntV  _ -> v
      | BoolV _ -> v
      | ListV  _ -> v
      | _      -> failwith ("Value of variable _" + x + "_ is not first-order.")

    | Prim1 (op, e1) ->
      match (op, e1) with
      | ("hd", SeqT e) -> eval (Item (1, e)) env
      | ("tl", SeqT e) -> eval (Item (List.length e, List e)) env
      | (_, _) -> let v1 = eval e1 env in
                    match (op, v1) with
                    | ("-", IntV i) -> IntV (- i)
                    | ("!", BoolV b) -> BoolV (not b)
                    | ("ise", ListV e) -> if (List.length e = 0) then (BoolV true) else (BoolV false)
                    | ("print", e) -> printf (val2string e)        // not sure how to handle    
                    | _ -> failwith "Impossible"

    | Prim2 (op, e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match (op, v1, v2) with
      | (";", _, _) -> v2                                          // could be wrong
      | ("=", _, _) -> BoolV (v1 = v2)
      | ("!=", _, _) -> BoolV (v1 <> v2)
      | ("::", _, ListV i2) -> SeqT ListV (v1 :: i2)               // might have issues with v1 values appending
      | ("&&", BoolV i1, BoolV i2) -> BoolV (i1 && i2)
      | ("<", IntV i1, IntV i2) -> BoolV (i1 < i2)
      | ("<=", IntV i1, IntV i2) -> BoolV (i1 <= i2)
      | ("/", IntV i1, IntV i2) -> IntV (i1 / i2)                  // floats might cause problem
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

    | Letrec (f, x, _, e1, _, e2) ->                               // might need additional changes
      let env2 = (f, Closure(f, x, e1, env)) :: env in
      eval e2 env2

    | Anon (t, s, e1) -> eval (Call(s, e1)) env                    // not sure about this one either

    | Match (e1, elist) -> let x = eval e1 env                     // probably contains errors
                           eval (findMatch x elist) env

    | Call (Var f, e1) ->
      let c = lookup env f in
      match c with
      | Closure (f, x, e2, fenv) ->
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