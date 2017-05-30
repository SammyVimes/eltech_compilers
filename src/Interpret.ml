open Language

(* Interpreter for expressions *)
module Expr =
  struct

    open Expr
    open Language.BinOp

    let rec eval feval expr st = 
      let eval' e = eval feval e st in
      match expr with
      | Var   x     -> (st x, [])
      | Const z     -> (z, [])
      | Binop  (op, x, y) -> 
        let (xv, xout) = eval' x in
        let (yv, yout) = eval' y in
        ((apply op) xv yv, xout @ yout)
      | Call (f, args) ->
       let args' = List.map (fun arg -> eval feval arg st) args in
       let args'' = List.map (fun (a, _) -> a) args' in
       let output = List.fold_left (fun l (_, out) -> l @ out) [] args' in
       let (res, eval_output) = feval f args'' in
       (res, output @ eval_output)

  end

(* Interpreter for statements *)
module Stmt =
  struct

    open Stmt
      
    let rec eval stmt ((conf, input, output) as c) =
      let (fenv, state) = conf in 
      let feval f args = 
            let fenv' x = List.assoc x fenv in
            let (fargs, fbody) = fenv' f in
            let state'' = List.map2 (fun ident arg -> (ident, arg)) fargs args in
            let (_, _, eout) = eval fbody ((fenv, state''), input, []) in
            (match eout with
             | []   -> failwith "Function should return a value"
             | eout -> let res::rout = List.rev eout in 
                       (res, List.rev rout))
      in
      let st x = List.assoc x state in
      match stmt with
      | Write   e     ->
        let (eres, eout) = Expr.eval feval e st in
        (conf, input, output @ eout @ [eres])
      | Skip          -> c
      | Seq (Return e, _) -> eval (Return e) c
      | Seq (s1, s2)  -> eval s1 c |> eval s2 
      | Assign (x, e) -> 
        let (eres, eout) = Expr.eval feval e st in
        ((fenv, (x, eres) :: state), input, output @ eout)
      | Read    x     -> 
	    let z :: input' = input in
	    ((fenv, (x, z) :: state), input', output)
      | If (e, s1, s2) -> 
        let (eres, eout) = Expr.eval feval e st in
        if (eres) <> 0 then (eval s1 (conf, input, output @ eout)) else (eval s2 (conf, input, output @ eout))
      (*eval self again but with new conf (which is eval'ed body of while')*)
      | While (e, s)   -> 
        let (eres, eout) = Expr.eval feval e st in
        let c' = (conf, input, output @ eout) in
        if (eres) <> 0 then eval stmt (eval s c') else c'
      | Fun (fname, fargs, fbody) -> 
        ((((fname, (fargs, fbody))::fenv), state), input, output)
      | Return e -> 
        let (eres, eout) = Expr.eval feval e st in
        (conf, input, output @ eout @ [eres])

  end

module Program =
  struct

    let eval p input = 
      let (_, _, output) = 
	Stmt.eval p (([], []), input, []) 
      in
      output

  end
