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
  
let matchout () =
  longident (!root_module ^ ".Internal.__match_out")
  
let error loc (s:string) =
  Location.raise_errorf ~loc "%s" s

let rec traverse f(*var wrapper*) g(*#tconst wrapper*) ({ppat_desc;ppat_loc;ppat_attributes} as patouter) =
  match ppat_desc with
  | Ppat_any -> f patouter
        (* _ *)
  | Ppat_var _ -> f patouter
        (* x *)
  | Ppat_alias (pat,tvarloc) -> {patouter with ppat_desc=Ppat_alias(traverse f g pat,tvarloc)}
        (* P as 'a *)
  | Ppat_constant _ -> patouter
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Ppat_interval (_,_) -> patouter
        (* 'a'..'z'

           Other forms of interval are recognized by the parser
           but rejected by the type-checker. *)
  | Ppat_tuple pats -> {patouter with ppat_desc=Ppat_tuple(List.map (traverse f g) pats)}
        (* (P1, ..., Pn)

           Invariant: n >= 2
        *)
  | Ppat_construct (lidloc,Some(pat)) -> {patouter with ppat_desc=Ppat_construct(lidloc,Some(traverse f g pat))}
  | Ppat_construct (_,None) -> patouter
        (* C                None
           C P              Some P
           C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
         *)
  | Ppat_variant (lab,Some(pat)) -> {patouter with ppat_desc=Ppat_variant(lab,Some(traverse f g pat))}
  | Ppat_variant (lab,None) -> patouter
        (* `A             (None)
           `A P           (Some P)
         *)
  | Ppat_record (recpats, Closed) -> {patouter with ppat_desc=Ppat_record(List.map (fun (r,p) -> (r,traverse f g p)) recpats, Closed)}
        (* { l1=P1; ...; ln=Pn }     (flag = Closed)
           { l1=P1; ...; ln=Pn; _}   (flag = Open)

           Invariant: n > 0
         *)
  | Ppat_array pats -> {patouter with ppat_desc=Ppat_array (List.map (traverse f g) pats)}
        (* [| P1; ...; Pn |] *)
  | Ppat_constraint (pat,typ)  -> {patouter with ppat_desc=Ppat_constraint(traverse f g pat,typ)}
        (* (P : T) *)
  | Ppat_type lidloc -> g lidloc
        (* #tconst *)
  | Ppat_lazy pat -> {patouter with ppat_desc=Ppat_lazy(traverse f g pat)}
                   
  | Ppat_record (_, Open)
  | Ppat_or (_,_) | Ppat_unpack _
  | Ppat_exception _ | Ppat_extension _ ->
       failwith (* ~loc:ppat_loc *) "%lin cannot handle this pattern"

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

let rec is_linpat {ppat_desc} = 
  match ppat_desc with
  | Ppat_type _ -> true
  | Ppat_alias (pat,_) -> is_linpat pat
  | Ppat_constraint (pat,_)  -> is_linpat pat
  | Ppat_any | Ppat_var _ 
  | Ppat_constant _ | Ppat_interval (_,_)
  | Ppat_tuple _ | Ppat_construct (_,_)
  | Ppat_variant (_,_) | Ppat_record (_, _)
  | Ppat_array _ | Ppat_lazy _ -> false
  | Ppat_or (_,_) | Ppat_unpack _
  | Ppat_exception _ | Ppat_extension _ ->
       failwith (* ~loc:ppat_loc *) "%lin cannot handle this pattern"
  
let lin_pattern oldpat =
  let wrap ({ppat_loc} as oldpat) =
    let lin_vars = ref []
    in
    let wrap_linpat ({loc} as linvar) =
      let newvar = newname "match" in
      lin_vars := (linvar,newvar) :: !lin_vars;
      pconstr ~loc "Linocaml.Lin__" [pvar ~loc newvar]
      
    and wrap_datapat ({ppat_loc} as pat) =
      pconstr ~loc:ppat_loc "Linocaml.Data__" [pat]
    in
    let newpat = traverse wrap_datapat wrap_linpat oldpat in
    let newpat =
      if is_linpat oldpat then
        newpat (* not to duplicate Lin__ pattern *)
      else
        pconstr ~loc:ppat_loc "Linocaml.Lin__" [newpat]
    in
    newpat, List.rev !lin_vars
  in
  let insert_expr (linvar, newvar) = app (setfunc ()) [Exp.ident linvar; evar newvar]
  in
  let newpat,lin_vars = wrap oldpat in
  newpat, List.map insert_expr lin_vars

let add_setslots es expr =
  List.fold_right (fun e expr -> app (monad_bind ()) [e; lam (punit ()) expr]) es expr

let expression_mapper id mapper exp attrs =
  let pexp_attributes = exp.pexp_attributes @ attrs in
  let pexp_loc=exp.pexp_loc in
  let process_inner expr = mapper.Ast_mapper.expr mapper expr
  in
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
      Some (process_inner { new_exp with pexp_loc; pexp_attributes })
  | "w", _ -> error pexp_loc "Invalid content for extension %w; it must be used as let%w"

  (* slot bind *)
  (* let%lin {lab} = e1 in e2 ==> Linocaml.Syntax.bind (e1 ~bindto:lab) (fun () -> e2) *)
  | "lin", Pexp_let (Nonrecursive, vbls, expr) ->
     let lin_binding ({pvb_pat;pvb_expr} as vb) =
         let newpat, inserts = lin_pattern pvb_pat in
         let new_expr = app (matchout ()) [pvb_expr] in
         {vb with pvb_pat=newpat;pvb_expr=new_expr}, inserts
     in
     let new_vbls, inserts = List.split (List.map lin_binding vbls) in
     let new_expr = add_setslots (List.concat inserts) expr in
     let make_bind {pvb_pat;pvb_expr;pvb_loc;pvb_attributes} expr =
       app ~loc:pexp_loc (monad_bind ()) [pvb_expr; lam ~loc:pvb_loc pvb_pat expr]
     in
     let expression = List.fold_right make_bind new_vbls new_expr
     in
     Some (process_inner expression)

  (* pattern match on linear part *)
  | "lin", Pexp_match(matched, cases) ->
     let lin_match ({pc_lhs=pat;pc_rhs=expr} as case) =
       let newpat, inserts = lin_pattern pat in
       let newexpr = add_setslots inserts expr in
       {case with pc_lhs=newpat;pc_rhs=newexpr}
     in
     let cases = List.map lin_match cases in
     let new_matched = app (matchout ()) [matched] in
     let new_exp = Pexp_apply(monad_bind (),[(Nolabel,new_matched); (Nolabel,Exp.function_ cases)])
     in
     Some (process_inner {pexp_desc=new_exp; pexp_loc; pexp_attributes})

  | "lin", _ -> error pexp_loc "Invalid content for extension %lin; it must be \"let%lin slotname = ..\" OR \"match%lin slotname with ..\""

  | _ -> None

let rebind_module modexpr =
  match modexpr.pmod_desc with
  | Pmod_ident {txt = id} -> root_module := String.concat "." (Longident.flatten id)
  | _ -> error modexpr.pmod_loc "Use (module M) here."
  
let runner ({ ptype_loc = loc } as type_decl) =
  match type_decl with
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
