

module PlcInterp

open Absyn
open Environ

let rec findMatch (v : expr) (elist: (expr option * expr) list) : expr =
  match v, elist with
  | v1, [] -> failwith "Match: no match found"
  | v1, (Some (eo), e1) :: t -> if (v1 = eo) then e1 else (findMatch v t)
  | v1, (None, e1) :: t -> e1

let rec eval (e : expr) (env : plcVal env) : plcVal =
    match e with
    | ConI i -> IntV i
    | ConB b -> BoolV b
    (* ESeq might not be needed in eval since it is a list of types and eval returns plcVal
    | ESeq s -> match s with                                       // definitely wrong
                | IntT i -> SeqV [IntV i]
                | h :: [] -> SeqV eval h env
                | h :: t -> SeqV List.append [eval h env] [eval t env]
    *)

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
      | ("hd", List elist) -> eval (List.item 1 elist) env                // do head and tail work for ESeq?
      | ("tl", List elist) -> eval (List.item (List.length elist) elist) env
      | (_, _) -> let v1 = eval e1 env in
                    match (op, v1) with
                    | ("-", IntV i) -> IntV (- i)
                    | ("!", BoolV b) -> BoolV (not b)
                    | ("ise", ListV elist) -> if (List.length elist = 0) then (BoolV true) else (BoolV false)
                    //| ("print", v) -> printf "%s" (val2string v)        // not sure how to handle because string    
                    | _ -> failwith "Impossible"

    | Prim2 (op, e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match (op, v1, v2) with
      | (";", _, _) -> v2                                          // could be wrong implementation
      | ("=", _, _) -> BoolV (v1 = v2)
      | ("!=", _, _) -> BoolV (v1 <> v2)
      | ("::", _, ListV i2) -> ListV (v1 :: i2)                    // should this return a sequence or list?
      | ("&&", BoolV i1, BoolV i2) -> BoolV (i1 && i2)
      | ("<", IntV i1, IntV i2) -> BoolV (i1 < i2)
      | ("<=", IntV i1, IntV i2) -> BoolV (i1 <= i2)
      | ("/", IntV i1, IntV i2) -> IntV (i1 / i2)                  // floats might cause problems
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

    | Letrec (f, _, x, _, e1, e2) ->                               // might need additional changes
      let env2 = (f, Clos(f, x, e1, env)) :: env in
      eval e2 env2

    | Anon (t, s, e1) -> eval (Call(Var s, e1)) env                // not sure about this one either

    | Match (e1, elist) -> eval (findMatch e1 elist) env           // probably contains errors

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