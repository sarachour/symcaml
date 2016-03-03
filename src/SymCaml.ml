open PyCamlWrapper
open Pycaml
open Sys
open Printf
open SymCamlData
open SymCamlParser
open SymCamlLexer

type symcaml = {
    w : PyCamlWrapper.wrapper ref;
    debug: bool ref;
}

module SymCaml :
sig
   val init : unit -> symcaml
   val set_debug : symcaml -> bool -> unit
   val define_symbol : symcaml ->  string -> symexpr
   val define_expr : symcaml -> string -> symexpr -> symexpr
   val define_wildcard: symcaml -> string -> symexpr list -> symexpr
   val define_function: symcaml -> string -> symexpr
   val clear : symcaml -> unit
   val expr2py : symcaml -> symexpr -> string

   val expand : symcaml -> symexpr -> symexpr
   val eval : symcaml -> symexpr -> symexpr
   val subs : symcaml -> symexpr -> (symexpr*symexpr) list -> symexpr
   val simpl : symcaml -> symexpr -> symexpr
   val eq : symcaml -> symexpr -> symexpr -> bool
   val pattern: symcaml -> symexpr -> symexpr -> ((string*symexpr) list) option
   val report : symcaml -> unit
end =
struct
   let _wr (s:symcaml) : PyCamlWrapper.wrapper ref = (s.w)
   let _wrc (s:symcaml) : PyCamlWrapper.wrapper = (!(_wr s))
   let _print (o:pyobject) : string = (PyCamlWrapper.cast_pyobj2str o)
   let _rprint (o:pyobject) : string = PyCamlWrapper.pyobj2repr o
   let dbg (s:symcaml) (fx: unit->unit) : unit =
      if !(s.debug) = true then
         begin
            fx();
            print_newline();
            ()
         end
      else ()

   let init () =
      let w = PyCamlWrapper.init ["sympy"] in
      let wr : PyCamlWrapper.wrapper ref = ref w in
      let dr : bool ref = ref (false) in
      {w=wr; debug=dr}

   let clear s =
      PyCamlWrapper.clear (_wr s)

   let set_debug s b =
      s.debug := b; ()

   let expr2py (s:symcaml) (e:symexpr) : string=
      let rec _expr2py (e:symexpr) : string =
         let exprlst2py (fn:string -> string -> string) (lst:symexpr list) : string =
            match lst with
            | h::t -> List.fold_right (fun x r -> let nx = _expr2py (Op1(Paren,x)) in fn nx r )  t (_expr2py (Op1(Paren,h)))
            | [] -> ""
         in
         let op12pyexpr op e = match op with
          | Integral(wrt) -> "Integral("^e^","^wrt^")"
          | Cos -> "cos("^e^")"
          | Sin -> "sin("^e^")"
          | Tan -> "tan("^e^")"
          | Paren -> "("^e^")"
          | NatExp -> "exp("^e^")"
          | Neg -> "-"^e
          | Deriv(wrt) -> "Derivative("^e^
             (List.fold_right (
                fun (v,n) r ->
                   let (sv,_) = PyCamlWrapper.get_var (_wr s) v in
                   let sn = string_of_int n in
                   r^","^sv^","^sn
             ) wrt "")^")"
        in
        let op2str x a b = match x with
         | Exp -> a^"**"^b
         | Div -> a^"/"^b
         | Eq -> "Eq("^a^","^b^")"
        in
        let opn2str x lst = match x with
        | Add -> exprlst2py (fun x r ->r^"+"^x) lst
        | Sub ->  exprlst2py (fun x r ->r^"-"^x) lst
        | Mult ->  exprlst2py (fun x r ->r^"*"^x) lst
        | Function(n) -> n^"("^(exprlst2py (fun x r ->r^","^x) lst)^")"
        in
         match e with
         | Symbol(name) -> let (n,obj) = PyCamlWrapper.get_var (_wr s) name in n
         | Op1(Paren,e) -> let ne = _expr2py (e) in op12pyexpr Paren ne
         | Op1(op,e) -> let ne = _expr2py (Op1(Paren,e)) in op12pyexpr op ne
         | Op2(op,e1,e2) -> let ne1 = _expr2py (Op1(Paren,e1)) and ne2 = _expr2py (Op1(Paren,e2)) in
          op2str op ne1 ne2
         | OpN(op,elst) ->  opn2str op elst
         | Decimal(x) -> string_of_float x
         | Integer(x) -> string_of_int x
      in
         _expr2py e

   let define_symbol (s:symcaml) (x:string) : symexpr =
      let cmd = "Symbol(\""^x^"\")" in
      dbg s (fun () -> Printf.printf "symbol: %s" cmd);
      let _ = PyCamlWrapper.define (_wr s) x cmd in
      (Symbol x)

   let define_function (s:symcaml) (x:string) : symexpr =
      let cmd = "Function(\""^x^"\")" in
      dbg s (fun () -> Printf.printf "define_function: %s" cmd);
      let _ = PyCamlWrapper.define (_wr s) x cmd in
      (Symbol x)

   let define_expr (s:symcaml) (x:string) (e:symexpr) =
      let stre = expr2py s e in
      let _ = PyCamlWrapper.define (_wr s) x stre in
      dbg s (fun () -> Printf.printf "define_expr: %s" stre);
      Symbol(x)

   let define_wildcard (s:symcaml) (x:string) (exns:symexpr list) : symexpr =
      let opt_arg = match exns with
         | h::t ->
            let get_arg (x:symexpr) = expr2py s x in
            let hn = get_arg h in
            let fn x r = let n = get_arg x in  r^","^n in
            "["^(List.fold_right fn t hn)^"]"
         | [] -> "[]"
      in
      let cmd ="Wild(\""^x^"\",exclude="^opt_arg^")" in
      dbg s (fun () -> Printf.printf "define_wildcard: %s" cmd);
      let _ = PyCamlWrapper.define (_wr s) x cmd in
      Symbol(x)

   let _pyobj2expr (s:symcaml) (p:pyobject) : symexpr =
      dbg s (fun () -> Printf.printf "[pyobj2expr] starting");
      match PyCamlWrapper.invoke (_wr s) "srepr" [p] [] with
         | Some(res) ->
            dbg s (fun () -> Printf.printf "[pyobj2expr] invoked");
            let strrep = PyCamlWrapper.cast_pyobj2str res in
            dbg s (fun () -> Printf.printf "[pyobj2expr] tostr. lexing: %s" strrep);
            begin
            try
               let lexbuf = Lexing.from_string strrep in
               dbg s (fun () -> Printf.printf "[pyobj2expr] lexed. parsing: %s" strrep);
               let result = SymCamlParser.main SymCamlLexer.main lexbuf in
               dbg s (fun () -> Printf.printf "[pyobj2expr] parsed: %s" (expr2py s result));
               result
            with
               |SymCamlData.SymCamlParserError(msg) -> raise (SymCamlException ("for equation:"^(strrep)^"\nparse error:"^msg))
               |e -> Printf.printf "%s\n----\n" strrep; raise e
            end
         | None -> raise (SymCamlFunctionException("pyobj2expr/srepr","unexpected: null result."))

   let get (s:symcaml) (e:string) =
      let (eval_expr,obj) = PyCamlWrapper.get_var (_wr s) e in
      let repr = _pyobj2expr s obj in
      repr

   let expand (s:symcaml) (e:symexpr) =
      let cmd = (expr2py s (Op1(Paren,e))) in
      let callee = PyCamlWrapper.eval  (_wr s) cmd in
      match callee with
         | Some(x) ->
            begin
            match PyCamlWrapper.invoke (_wr s)  "expand" [x] [] with
               | Some(res) -> let repr = _pyobj2expr s res in repr
               | None ->  raise (SymCamlFunctionException("eval","unexpected: null result."))
            end
         | None -> raise (SymCamlFunctionException("expand","unexpected: null callee."))

   let eval (s:symcaml) (e:symexpr) : symexpr =
      let cmd = (expr2py s (Op1(Paren,e))) in
      dbg s (fun () -> Printf.printf "eval: %s" cmd);
      match PyCamlWrapper.eval (_wr s) cmd with
         | Some(callee) ->
         begin
            match PyCamlWrapper.invoke_from (_wr s) callee  "doit" [] [] with
            | Some(res) -> let repr = _pyobj2expr s res in repr
            | None -> raise (SymCamlFunctionException("eval","unexpected: null result."))
         end
         | None -> raise (SymCamlFunctionException("eval","unexpected: null callee."))
      (*
      let repr = _pyobj2expr s res in
      repr
      *)
   (*
   srepr input beforehand
   *)
   let subs (s:symcaml) (e:symexpr) (sub:(symexpr*symexpr) list) : symexpr =
      dbg s (fun () -> Printf.printf "subs: beginning");
      let callee : pyobject=
         match PyCamlWrapper.eval (_wr s) (expr2py s (Op1(Paren,e))) with
         |Some(obj) -> obj
         |None ->raise (SymCamlFunctionException("subs","unexpected: callee cannot be null."))
      in
      let to_tuple ((a,b):symexpr*symexpr) : string =
         let stct = "("^(expr2py s a)^","^(expr2py s b)^")" in
         stct
      in
      let to_tuple_list lst = match lst with
         |[v] -> "["^(to_tuple v)^"]"
         |v::t -> "["^(List.fold_right (fun x r -> r^","^(to_tuple x)) t (to_tuple v))^"]"
         |[] -> "[]"
      in
      let args : pyobject = match PyCamlWrapper.eval (_wr s) (to_tuple_list sub) with
         |Some(obj) -> obj
         |None ->raise (SymCamlFunctionException("subs", "unexpected: sub list cannot be null."))
      in
      dbg s (fun () -> Printf.printf "subs: %s using %s" (expr2py s (Op1(Paren,e))) (to_tuple_list sub));
      match PyCamlWrapper.invoke_from (_wr s) callee "subs" [args] [] with
      | Some(res) ->
         let repr = _pyobj2expr s res in repr
      | None -> raise (SymCamlFunctionException("subs","unexpected: null result."))




   let simpl (s:symcaml) (e:symexpr) =
      let cmd = (expr2py s (Op1(Paren,e))) in
      let callee = PyCamlWrapper.eval  (_wr s) cmd in
         match callee with
         | Some(x) ->
            begin
            match PyCamlWrapper.invoke (_wr s)  "simplify" [x] [] with
               | Some(res) ->  let repr = _pyobj2expr s res in repr
               | None -> raise (SymCamlFunctionException("simpl","unexpected: null result."))
            end
         | None-> raise (SymCamlFunctionException("simpl","unexpected: null callee."))

   let eq (s:symcaml) (lhs:symexpr) (rhs:symexpr) =
      let expr = OpN(Sub,[Op1(Paren,lhs);Op1(Paren,rhs)]) in
      let res = simpl s expr in
      match res with
      | Integer(0) -> true
      | _ -> false

   let pattern (s:symcaml) (e:symexpr) (pat: symexpr) : ((string*symexpr) list) option =
      let transform (key,v) : (string*symexpr) =
         dbg s (fun () -> Printf.printf "[pattern]/el starting");
         let nk : string= _rprint key in
         dbg s (fun () -> Printf.printf "[pattern]/el convert val: %s" nk);
         let expr : symexpr = _pyobj2expr s v in
         dbg s (fun () -> Printf.printf "[pattern]/el convert key: %s" (expr2py s expr));
         let realname = PyCamlWrapper.find_var (_wr s) key in
         dbg s (fun () -> Printf.printf "[pattern]/el get realname: %s" realname);
         (realname,expr)
      in
      let ecmd = (expr2py s (Op1(Paren,e))) in
      let patcmd = (expr2py s (Op1(Paren,pat))) in
      dbg s (fun () -> Printf.printf "[pattern] match: %s -> %s" ecmd patcmd);
      let eobj = PyCamlWrapper.eval  (_wr s) ecmd in
      dbg s (fun () -> Printf.printf "[pattern] eval: %s" ecmd);
      let patobj = PyCamlWrapper.eval  (_wr s) patcmd in
      dbg s (fun () -> Printf.printf "[pattern] eval: %s" patcmd);
         match (eobj,patobj) with
         | (Some(texpr),Some(tpat)) ->
            begin
               dbg s (fun () -> Printf.printf "[pattern] invoke: %s" patcmd);
               match PyCamlWrapper.invoke_from (_wr s)  texpr "match" [tpat] [] with
               | Some(res) ->
                  begin
                  dbg s (fun () -> Printf.printf "[pattern] some result: %s" (PyCamlWrapper.pyobj2str res));
                  let assigns = PyCamlWrapper.pydict2ml res transform in
                     dbg s (fun () -> Printf.printf "[pattern] done.");
                     Some assigns
                  end
               | None -> None
            end
         | _ -> raise (SymCamlFunctionException("pattern","unexpected: null callee or argument."))

   let report (s:symcaml) : unit =
      PyCamlWrapper.report (_wr s)

end
