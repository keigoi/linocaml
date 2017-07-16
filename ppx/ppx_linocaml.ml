open Ast_helper
open Asttypes
open Parsetree
open Longident
open Ast_convenience

let newname =
  let r = ref 0 in
  fun prefix ->
    let i = !r in
    r := i + 1;
  Printf.sprintf "__ppx_linocaml_%s_%d" prefix i
  
let root_module = ref "Linocaml.Syntax"

let longident str = Exp.ident (lid str)

let monad_bind () =
  longident (!root_module ^ ".bind")
  
let setfunc () =
  longident (!root_module ^ ".set")
  
let unsetfunc () =
  longident (!root_module ^ ".Internal.__unset")
  
let getfunc () =
  longident (!root_module ^ ".Internal.__get")
  
let emptyslot () =
  longident (!root_module ^ ".Internal.__empty")
  
let runmonad () =
  longident (!root_module ^ ".Internal.__run")
  
let error loc (s:string) =
  Location.raise_errorf ~loc "%s" s
  
(* [?? = e0] and [?? = e1] and .. ==> [dum$0 = e0] and [dum$1 = e1] and ..
   (to use with bindbody_of_let.) *)
let bindings_of_let bindings =
  List.mapi (fun i binding ->
      {binding with pvb_pat = pvar (newname "let")}
    ) bindings

(* [p0 = ??] and [p1 = ??] and .. and e ==> [bind dum$0 (fun p0 -> bind dum$1 (fun p1 -> .. -> e))] *)
let bindbody_of_let exploc bindings exp =
  let rec make i bindings =
    match bindings with
    | [] -> exp
    | binding :: t ->
      let name = (evar (newname "let")) [@metaloc binding.pvb_expr.pexp_loc] in
      let f = [%expr (fun [%p binding.pvb_pat] -> [%e make (i+1) t])] [@metaloc binding.pvb_loc] in
      let new_exp = [%expr [%e monad_bind ()] [%e name] [%e f]] [@metaloc exploc] in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in
  make 0 bindings

(* [{lab1} = e1] and [{lab2} = e2 and .. and e ==> e1 ~bindto:lab1 >>= (fun () -> e2 ~bindto:lab2 ..)  *)
(* [#lab1 = e1] and [#lab2 = e2 and .. and e ==> e1 ~bindto:lab1 >>= (fun () -> e2 ~bindto:lab2 ..)  *)
let slot_bind bindings expr =
  let f binding expr =
    match binding with
    | {pvb_pat = {ppat_desc = Ppat_record ([({txt},_)],Closed)}; pvb_expr = rhs}
    | {pvb_pat = {ppat_desc = Ppat_type {txt}}; pvb_expr = rhs} ->
      let lensname = String.concat "." (Longident.flatten txt) in
      let f = Exp.fun_ Label.nolabel None (punit ()) expr in
      [%expr [%e monad_bind ()] ([%e rhs] ~bindto:[%e evar lensname]) [%e f]]
    | _ -> raise Not_found
  in List.fold_right f bindings expr
  
(* Converts match clauses to handle branching.
  | `lab1(p11,..,p1N) -> e_1
  | ..
  | `labM(pM1,..,pMN) -> e_M
  ==>
  | `lab1(p11',..,p1N') -> e_1'
  | ..
  | `labN(pM1',..,pMN') -> e_M'
  where, pXY' = fresh_pat() if pXY == #slotname, and we insert '(set e_slot new_pat) >>' before e_X
         pXY' = (Val pXY)   otherwise
*)
let linocaml_branch_clauses e_slot cases =
  let convpat = function
    | {ppat_desc=Ppat_type(name);ppat_loc;ppat_attributes} ->
       let var = newname "match" in
       let linvar = pconstr ~loc:ppat_loc "Linocaml.Lin" [pvar var] in
       let insert_exp = [%expr [%e setfunc ()] [%e Exp.ident name] (Linocaml.Lin [%e longident var])] in
       linvar, [insert_exp]
    | pat ->
       let valpat = [%pat? Linocaml.Data [%p pat]] in
       valpat, []
  in
  (* let add_unset orig = [%expr [%e monad_bind ()] ([%e unsetfunc ()] [%e e_slot]) (fun () -> [%e orig])] in *)
  let insert_bind orig inserts =
    List.fold_right (fun exp expr -> [%expr [%e monad_bind ()] [%e exp] (fun () -> [%e expr])]) inserts orig
  in
  let convcase = function
    | {pc_lhs={ppat_desc=Ppat_variant(labl,pat);ppat_loc;ppat_attributes} as lhs_orig;pc_guard;pc_rhs=rhs_orig} ->
       begin match pat with
       | Some {ppat_desc=Ppat_tuple(pats);ppat_loc=loc_in;ppat_attributes=attr_in} ->
          let pat_new,inserts = List.split (List.map convpat pats) in
          let pat_new = {ppat_desc=Ppat_variant(labl,Some({ppat_desc=Ppat_tuple(pat_new);ppat_loc=loc_in;ppat_attributes=attr_in}));ppat_loc;ppat_attributes} in
          let pat_new = pconstr ~loc:loc_in "Linocaml.Lin" [pat_new] in
          let inserts = List.concat inserts in
          let rhs_new = insert_bind rhs_orig inserts in
          {pc_lhs=pat_new;
           pc_guard;
           pc_rhs=rhs_new}
       | Some(pat) ->
          let pat_new,inserts = convpat pat in
          let pat_new = {ppat_desc=Ppat_variant(labl,Some(pat_new));ppat_loc;ppat_attributes} in
          let pat_new = pconstr ~loc:ppat_loc "Linocaml.Lin" [pat_new] in
          let rhs_new = insert_bind rhs_orig inserts in
          {pc_lhs=pat_new;pc_guard;pc_rhs=rhs_new}
       | None ->
          let pat_new = pconstr ~loc:ppat_loc "Linocaml.Lin" [lhs_orig] in
          {pc_lhs=pat_new;pc_guard;pc_rhs=rhs_orig}
       end
    | {pc_lhs={ppat_loc=loc}} -> error loc "Invalid pattern"
  in
  List.map convcase cases

(* let branch_func_name funname = longident (!root_module ^ ".LinocamlN.__"^funname) *)

(* let make_branch_func_types labls = *)
(*   let open Typ in *)
(*   let rows = *)
(*     List.mapi (fun i labl -> Rtag(labl,[],false,[var (freshname ())])) labls *)
(*   in *)
(*   [%type: [%t (variant rows Closed None)] * [%t var (freshname ())] -> [%t var (freshname ())] ] *)

let expression_mapper id mapper exp attrs =
  let pexp_attributes = exp.pexp_attributes @ attrs in
  let pexp_loc=exp.pexp_loc in
  match id, exp.pexp_desc with

  (* monadic bind *)
  (* let%s p = e1 in e2 ==> let dum$0 = e1 in Linocaml.Syntax.bind dum$0 e2 *)
  | "w", Pexp_let (Nonrecursive, vbl, expression) ->
      let new_exp =
        Exp.let_
          Nonrecursive
          (bindings_of_let vbl)
          (bindbody_of_let exp.pexp_loc vbl expression)
      in
      Some (mapper.Ast_mapper.expr mapper { new_exp with pexp_loc; pexp_attributes })
  | "w", _ -> error pexp_loc "Invalid content for extension %s|%w"

  (* slot bind *)
  (* let%lin {lab} = e1 in e2 ==> Linocaml.Syntax.bind (e1 ~bindto:lab) (fun () -> e2) *)
  | "slot", Pexp_let (Nonrecursive, vbl, expression) ->
      let new_exp = slot_bind vbl expression in
      Some (mapper.Ast_mapper.expr mapper { new_exp with pexp_loc; pexp_attributes })
  | "slot", _ -> error pexp_loc "Invalid content for extension %lin"

  (* pattern match on linear part *)
  | "lin", Pexp_match(e_slot, cases) ->
     let open Typ in
     let cases = linocaml_branch_clauses e_slot cases in
     let branch_exp = [%expr [%e getfunc ()] [%e e_slot]] in
     let new_exp = Pexp_apply(monad_bind (),[(Nolabel,branch_exp); (Nolabel,Exp.function_ cases)])
     in
     Some (mapper.Ast_mapper.expr mapper {pexp_desc=new_exp; pexp_loc; pexp_attributes})
  | "lin", _ -> error pexp_loc "Invalid content for extension %lin; it must be match%lin slotname with .."

  | _ -> None

let rebind_module modexpr =
  match modexpr.pmod_desc with
  | Pmod_ident {txt = id} -> root_module := String.concat "." (Longident.flatten id)
  | _ -> error modexpr.pmod_loc "Use (module M) here."
  
let runner ({ ptype_loc = loc } as type_decl) =
  match type_decl with
  (* | {ptype_kind = Ptype_record labels} -> *)
  | {ptype_name = {txt = name}; ptype_manifest = Some ({ptyp_desc = Ptyp_object (labels, Closed)})} ->
    let obj =
      let meth (fname,_,_) =
        {pcf_desc =
           Pcf_method ({txt=fname;loc=Location.none},
                       Public,
                       Cfk_concrete(Fresh, emptyslot ()));
         pcf_loc = Location.none;
         pcf_attributes = []}
      in
      Exp.object_ {pcstr_self = Pat.any (); pcstr_fields = List.map meth labels}
    in
    let mkfun = Exp.fun_ Label.nolabel None in
    let runner = mkfun (pvar "x") (app (runmonad ()) [evar "x"; obj]) in
    let quoter = Ppx_deriving.create_quoter () in
    let varname = "run_" ^ name in
    [{pstr_desc = Pstr_value (Nonrecursive, [Vb.mk (pvar varname) (Ppx_deriving.sanitize ~quoter runner)]); pstr_loc = Location.none}]
  | _ -> error loc "run_* can be derived only for record or closed object types"

let has_runner attrs =
  List.exists (fun ({txt = name},_) -> name = "runner")  attrs
       
let mapper_fun _ =
  let open Ast_mapper in
  let expr mapper outer =
  match outer with
  | {pexp_desc=Pexp_extension ({ txt = id }, PStr([{pstr_desc=Pstr_eval(inner,inner_attrs)}])); pexp_attributes=outer_attrs} ->
     begin match expression_mapper id mapper inner (inner_attrs @ outer_attrs) with
     | Some exp -> exp
     | None -> default_mapper.expr mapper outer
     end
  | _ -> default_mapper.expr mapper outer
  and stritem mapper outer =
    match outer with
    | {pstr_desc = Pstr_type (_, type_decls)} ->
       let runners =
         List.map (fun type_decl ->
           if has_runner type_decl.ptype_attributes then
             [runner type_decl]
           else []) type_decls
       in [outer] @ List.flatten (List.flatten runners)
    | _ -> [default_mapper.structure_item mapper outer]
  in
  let structure mapper str =
    List.flatten (List.map (stritem mapper) str)
  in
  {default_mapper with expr; structure}
