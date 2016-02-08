
exception SymCamlParserError of string;;
exception SymCamlException of string;;
exception SymCamlFunctionException of string*string;;
exception PyCamlWrapperException of string;;

type symvar = string


type op2 =
  | Exp
  | Div
  | Eq

type op1 =
  | Paren
  | Cos
  | Sin
  | Tan
  | NatExp
  | Deriv of (symvar*int) list
  | Integral of symvar
  | Neg

type opn =
  | Sub
  | Add
  | Mult
  | Function of symvar


type symexpr =
   | Symbol of symvar
   | Op1 of op1*symexpr
   | Op2 of op2*symexpr*symexpr
   | OpN of opn*(symexpr list)
   | Decimal of float
   | Integer of int



module SymExpr :
sig
   val get_vars: symexpr -> string list
   val expr2str: symexpr -> string
end =
struct

   let expr2str (e:symexpr) : string=
      let rec _expr2str (e:symexpr) : string =
         let exprlst2str (fn:'a -> string -> string) (lst:'a list) : string =
            match lst with
            | h::t -> List.fold_right fn  t (_expr2str h)
            | [] -> ""
         in
         let op2str x a b = match x with
          | Exp -> a^"^"^b
          | Div -> a^"/"^b
          | Eq -> "Eq("^a^","^b^")"
         in
         let op1str x a = match x with
          | Paren -> "("^a^")"
          | Cos -> "cos("^a^")"
          | Sin -> "sin("^a^")"
          | Tan -> "tan("^a^")"
          | Neg -> "-("^a^")"
          | NatExp -> "exp("^a^")"
          | Deriv(wrt) -> "Derivative("^a^
             (List.fold_right (
                fun (v,n) r ->
                   let sn = string_of_int n in
                   r^","^v^","^sn
             ) wrt "")^")"
          | Integral(wrt) -> "Integral("^(a)^","^(wrt)^")"
         in
         let opnstr x lst = match x with
          | Add -> exprlst2str (fun x r ->r^"+"^(_expr2str x)) lst
          | Sub -> exprlst2str (fun x r ->r^"-"^(_expr2str x)) lst
          | Mult -> exprlst2str (fun x r ->r^"*"^(_expr2str x)) lst
          | Function(n) -> n^"("^(exprlst2str (fun x r ->r^","^(_expr2str x)) lst)^")"
         in
         match e with
         | Symbol(name) -> name
         | Op1(op,ex) -> let nex = _expr2str ex in op1str op nex
         | Op2(op,e1,e2) -> let ne1 = _expr2str e1 and ne2 = _expr2str e2 in
            op2str op ne1 ne2
         | Decimal(x) -> string_of_float x
         | Integer(x) -> string_of_int x
         | OpN(opn,en) -> opnstr opn en 
      in
         _expr2str e

   let rec get_vars (s:symexpr) : string list =
      let get_vars_explst (e:symexpr list) =
         List.fold_right (fun x r-> (get_vars x) @ r) e []
      in
      match s with
      | Symbol(e) -> [e]
      | Op1(_, e) -> get_vars e
      | Op2(_,e1,e2) -> get_vars_explst [e1;e2]
      | OpN(_,elst) -> get_vars_explst elst
      | Decimal(_) -> []
      | Integer(_) -> []

end
