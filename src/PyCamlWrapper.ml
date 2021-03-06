open Pycaml
open Sys
open Printf
open SymCamlData
(* Check out sympy *)
module PyCamlWrapper :
sig
   type wrapper = {
      mutable main: pyobject;
      mutable venv: pyobject;
      mutable tmp: pyobject;
   }
   val print_info : unit -> unit
   val init: string option -> string option -> (string) list -> wrapper
   val get_python_home : unit -> string
   val define: wrapper ref -> string -> string -> pyobject
   val define_tmp_var: wrapper ref -> string -> string -> pyobject
   val invoke: wrapper ref -> string -> pyobject list -> (string*pyobject) list -> pyobject option
   val invoke_from: wrapper ref -> pyobject -> string -> pyobject list -> (string*pyobject) list -> pyobject option
   val clear: wrapper ref -> unit
   val eval: wrapper ref -> string -> pyobject option
   val to_env_var : string -> string
   val get_var : wrapper ref -> string -> (string*pyobject)
   val get_tmp_var : wrapper ref -> string -> (string*pyobject)
   val list2tuple: pyobject list -> pyobject
   val cast_pyobj2str: pyobject -> string
   val pyobj2str: pyobject -> string
   val pyobj2bool : pyobject -> bool
   val pyobj2repr: pyobject -> string
   val pydict2ml: pyobject -> (pyobject*pyobject -> 'a*'b) -> (('a*'b) list)
   val find_var : wrapper ref -> pyobject -> string
   val report: wrapper ref -> unit
   val is_null : pyobject -> bool

end =
struct
   type wrapper = {
      mutable main: pyobject;
      mutable venv: pyobject;
      mutable tmp: pyobject;
   }
   let null = pynull()
   let none = pynone()


   let handle_err () : unit =
         let typ = null in
         let value = null in
         let trace = null in
         let typ = pyerr_occurred () in
         if typ <> null then
           begin
			       pyerr_print();
			       pyerr_printex 1;
			       raise (PyCamlWrapperException ("python error:"))
           end
         else
           ()

   let run x =
      let _ = pyrun_simplestring(x) in
      handle_err();
      ()


   let _env x = "env[\""^x^"\"]"
   let _tmp x = "tmp[\""^x^"\"]"
   let _uw w = !w
   let _get_dict_val (d:pyobject) (k:string) : pyobject option =
     let x = pydict_getitemstring(d,k) in
      handle_err();
      if x = null or x = none then None else Some(x)

   let _get_obj_val (o:pyobject) (attr:string) : pyobject option =
      let x = pyobject_getattrstring(o,attr) in
      handle_err();
      if x = null then None else Some(x)

   let _throw_if_null (tag:string) (s:'a option) : 'a = match s with
      | Some(x) -> x
      | None -> raise (PyCamlWrapperException (tag^": unexpected null value."))

   let _upd (w:wrapper ref) =
      let mdl = pyimport_addmodule("__main__") in
      let venv = _throw_if_null "update env" (_get_obj_val mdl "env") in
      let tmp = _throw_if_null "update tmp"(_get_obj_val mdl "tmp") in
      let nw = {venv=venv; tmp=tmp; main=mdl} in
      w := nw

   let _clrtmp w = run("tmp = {}"); _upd w


   let is_null e = e = null

   let print_info () =
      Printf.printf "At least python 3.1 required.\n";
      Printf.printf "prefix:  %s\n" (py_getprefix());
      Printf.printf "eprefix: %s\n" (py_getexecprefix());
      Printf.printf "init: %d\n" (py_isinitialized());
      Printf.printf "path: %s\n" (py_getpath());
      Printf.printf "prog: %s\n" (py_getprogramname());
      Printf.printf "version: %s\n" (py_getversion());
      Printf.printf "----------"





   let cast_pyobj2str (o:pyobject):string =
      let x = pystring_asstring o in
      x

   let pyobj2int (o:pyobject) : int =
      let x = pyint_asint o in
      x

   let pyobj2bool (o:pyobject) : bool =
      let x = pyobj2int o in
      if x = 0 then
        true
      else
        false

   let pyobj2str (o:pyobject):string =
      let so = pyobject_str o in
      let s = cast_pyobj2str so in
      s

   let pyobj2repr (o:pyobject):string =
      let x = pyobject_repr o in
      let s = cast_pyobj2str x in
      s

   let list2tuple (lst:pyobject list): pyobject =
      if List.length lst  = 0 then pytuple_empty
      else
         let arr = Array.of_list lst in
         let tup = pytuple_fromarray arr in
         tup

   let pydict2ml (obj:pyobject) (fxn:(pyobject*pyobject)->('a*'b)) : ('a*'b) list =
		 let okeys = pydict_keys obj in
		 let _ = handle_err() in
		 let arrkeys = pylist_toarray okeys in
		 let _ = handle_err() in
		 let keys = Array.to_list arrkeys in
		 let efun k =
			let v = pydict_getitem(obj,k) in
			let _ = handle_err() in
			(k,v)
		 in
         let elems = List.map efun keys in
         List.map fxn elems

   let report w : unit =
      run("print repr(env);");
      run("print repr(tmp);")

   let get_python_home () : string =
     let home : string = py_getpythonhome () in
     home

   

   let init (prog:string option) (home:string option) (imports :(string) list) : wrapper =
     let modulename = "sympy" in
     begin
       match prog with
       | Some(prog) -> py_setprogramname(prog)
       | None -> ()
     end;
     begin
       match home with
       | Some(home) -> py_setpythonhome(home)
       | None -> ()
     end;
     handle_err();
     py_initialize();
     handle_err();
     print_info();
     handle_err();
     List.map (fun (x) -> run(x)) imports;
     run("env = {}");
     run("tmp = {}");
     let mdl = pyimport_addmodule("__main__") in
     handle_err();
     let venv = _throw_if_null "init env" (_get_obj_val mdl "env") in
     let tmp = _throw_if_null "init tmp" (_get_obj_val mdl "tmp") in
     {main=mdl;venv=venv; tmp=tmp}

   let eval (w:wrapper ref) (cmd) : pyobject option =
      let _ = _clrtmp w in
      let tvar = "__tmp__" in
      let cmd = (_tmp tvar)^"="^cmd in
      run(cmd);
      _upd w;
      let res = pydict_getitemstring((_uw w).tmp, tvar) in
      if res = null then None else Some(res)


   let to_env_var (name:string) =
     _env name

   let get_var (w:wrapper ref) (name:string) =
      let n = _env name in
      let obj = match _get_dict_val (_uw w).venv name  with
         | Some(x) -> x
         | None -> raise (PyCamlWrapperException ("variable "^name^" not found in environment."))
      in
      (n,obj)

   let get_tmp_var (w:wrapper ref) (name:string) =
      let n = _tmp name in
      let obj = match _get_dict_val (_uw w).tmp name  with
         | Some(x) -> x
         | None -> raise (PyCamlWrapperException ("variable "^name^" not found in environment."))
      in
      (n,obj)

   let find_var (w:wrapper ref) (obj: pyobject) =
      let compose ((k,v):pyobject*pyobject) : string*pyobject =
         let key = cast_pyobj2str k in
         let vl = v in
         (key,vl)
      in
      let cmpobjs (name,inst) =
         let r = pyobject_richcomparebool(inst,obj,(opid2int Py_EQ)) in
         r <> 0
      in
      let evl = pydict2ml (_uw w).venv compose in
      match List.filter cmpobjs evl with
         |[(name,_)] -> name
         | [] -> raise (PyCamlWrapperException ("variable resembling argument not found in environment."))
         | _ -> raise (PyCamlWrapperException ("argument has multiple variable names."))

   let clear (w:wrapper ref) =
      run("env = {}");
      run("tmp = {}");
      _upd w;
      ()


   let define (w:wrapper ref) (vname:string) (cmd:string) : (pyobject) =
      let evname = (_env vname) in
      let _ = eval w (evname^"="^cmd) in
      _upd w;
      let obj = match _get_dict_val (_uw w).venv vname with
         | Some(x) -> x
         | None -> raise (PyCamlWrapperException ("variable "^vname^" could not be defined. not found."))
      in
      obj

   let define_tmp_var (w:wrapper ref) (vname:string) (cmd:string) : (pyobject) =
      let evname = (_tmp vname) in
      let _ = eval w (evname^"="^cmd) in
      _upd w;
      let obj = match _get_dict_val (_uw w).tmp vname with
         | Some(x) -> x
         | None -> raise (PyCamlWrapperException ("variable "^vname^" could not be defined. not found."))
      in
      obj

   let _invoke (w:wrapper ref) (callee:pyobject) (fxnname:string) (args:pyobject list) (kwargs:(string*pyobject) list) : pyobject option =
      let fargs =  list2tuple args in
      if is_null callee then
         raise (PyCamlWrapperException ("callee is null on invocation of "^fxnname))
      else if is_null fargs then
         raise (PyCamlWrapperException ("null object passed in as args for "^fxnname))
      else
         let pyfxn = _get_obj_val callee fxnname in
         begin
         handle_err();
         match pyfxn with
            | None -> raise (PyCamlWrapperException ("could not find function '"^fxnname^"'." ))
            | Some(fn) ->
               begin
               let result = pyeval_callobjectwithkeywords(fn,fargs,null) in
                  handle_err();
                  if result = null || result = none then None else Some(result)
               end

         end
   let invoke (w:wrapper ref) n args kwargs : pyobject option =  _invoke w (_uw w).main n args kwargs


   let invoke_from (w:wrapper ref) o n args kwargs : pyobject option =  _invoke w o n (args) kwargs




end
