(* TODO: replace "failwith" with proper error-handling *)

open Asttypes
open Longident
open Parsetree
open Ast_helper
open Ast_convenience

let newname =
  let r = ref 0 in
  fun prefix ->
    let i = !r in
    r := i + 1;
  Printf.sprintf "__ppx_linocaml_%s_%d" prefix i

let root_module = ref "Syntax"

let longident ?loc str = evar ?loc str

let monad_bind () =
  longident (!root_module ^ ".bind")

let emptyslot () =
  longident (!root_module ^ ".empty")

let monad_bind_raw () =
  longident (!root_module ^ ".Internal.__bind_raw")

let monad_return_raw () =
  longident (!root_module ^ ".Internal.__return_raw")

let mkbindfun () =
  longident (!root_module ^ ".Internal.__mkbindfun")

let putval_raw () =
  longident (!root_module ^ ".Internal.__putval_raw")

let takeval_raw () =
  longident (!root_module ^ ".Internal.__takeval_raw")

let runmonad () =
  longident (!root_module ^ ".Internal.__run")

let disposeenv () =
  longident (!root_module ^ ".Internal.__dispose_env")

let error loc (s:string) =
  Location.raise_errorf ~loc "%s" s


let add_putval es expr =
  let insert_expr (linvar, newvar) =
    app (* ~loc:oldpat.ppat_loc *) (putval_raw ()) [Exp.ident ~loc:linvar.loc linvar; evar ~loc:linvar.loc newvar]
  in
  List.fold_right (fun e expr ->
      app
        (monad_bind_raw ())
        [insert_expr e; lam (punit ()) expr]) es expr

let add_takeval es expr =
  List.fold_right (fun (v,slot) expr ->
      app
        (monad_bind_raw ())
        [app (takeval_raw ()) [slot];
         lam (pvar v) expr]) es expr

let rec traverse_pats f(*var wrapper*) g(*#tconst wrapper*) ({ppat_desc} as patouter) =
  match ppat_desc with
  | Ppat_any -> f patouter
        (* _ *)
  | Ppat_var _ -> f patouter
        (* x *)
  | Ppat_alias (pat,tvarloc) ->
     error tvarloc.loc "as-pattern is forbidden at %lin match" (* TODO relax this *)
     (* {patouter with ppat_desc=Ppat_alias(traverse_pats f g pat,tvarloc)} *)
        (* P as 'a *)
  | Ppat_constant _ -> patouter
        (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
  | Ppat_interval (_,_) -> patouter
        (* 'a'..'z'

           Other forms of interval are recognized by the parser
           but rejected by the type-checker. *)
  | Ppat_tuple pats -> {patouter with ppat_desc=Ppat_tuple(List.map (traverse_pats f g) pats)}
        (* (P1, ..., Pn)

           Invariant: n >= 2
        *)
  | Ppat_construct (lidloc,Some(pat)) -> {patouter with ppat_desc=Ppat_construct(lidloc,Some(traverse_pats f g pat))}
  | Ppat_construct (_,None) -> patouter
        (* C                None
           C P              Some P
           C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
         *)
  | Ppat_variant (lab,Some(pat)) -> {patouter with ppat_desc=Ppat_variant(lab,Some(traverse_pats f g pat))}
  | Ppat_variant (lab,None) -> patouter
        (* `A             (None)
           `A P           (Some P)
         *)
  | Ppat_record (recpats, Closed) ->
     {patouter with
       ppat_desc=Ppat_record(List.map (fun (field,pat) -> (field,traverse_pats f g pat)) recpats, Closed)
     }
        (* { l1=P1; ...; ln=Pn }     (flag = Closed)
           { l1=P1; ...; ln=Pn; _}   (flag = Open)

           Invariant: n > 0
         *)
  | Ppat_array pats -> {patouter with ppat_desc=Ppat_array (List.map (traverse_pats f g) pats)}
        (* [| P1; ...; Pn |] *)
  | Ppat_constraint (pat,typ)  -> {patouter with ppat_desc=Ppat_constraint(traverse_pats f g pat,typ)}
        (* (P : T) *)
  | Ppat_type lidloc -> g lidloc
        (* #tconst *)
  | Ppat_lazy pat -> {patouter with ppat_desc=Ppat_lazy(traverse_pats f g pat)}

  | Ppat_record (_, Open)
  | Ppat_or (_,_) | Ppat_unpack _
  | Ppat_exception _ | Ppat_extension _ | Ppat_open _ ->
       error patouter.ppat_loc "%lin cannot handle this pattern"

let rec is_linpat {ppat_desc;ppat_loc} =
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
    | Ppat_exception _ | Ppat_extension _ | Ppat_open _ ->
     error ppat_loc "%lin cannot handle this pattern"

(* (#p, #q, x) ==> (__tmp1, __tmp2, x), [(#p, "__tmp1"), (#q, "__tmp2")] *)
let lin_pattern oldpat : pattern * (Longident.t Location.loc * string) list=
  let wrap ({ppat_loc} as oldpat) =
    let lin_vars = ref []
    in
    let replace_linpat ({loc} as linvar) =
      let newvar = newname "match" in
      lin_vars := (linvar,newvar) :: !lin_vars;
      pconstr ~loc "Linocaml.Base.Lin_Internal__" [pvar ~loc newvar]

    and wrap_datapat ({ppat_loc} as pat) =
      pconstr ~loc:ppat_loc "Linocaml.Base.Data_Internal__" [pat]
    in
    let newpat = traverse_pats wrap_datapat replace_linpat oldpat in
    let newpat =
      if is_linpat oldpat then
        newpat (* not to duplicate Lin pattern *)
      else
        pconstr ~loc:ppat_loc "Linocaml.Base.Lin_Internal__" [newpat]
    in
    newpat, List.rev !lin_vars
  in
  let newpat,lin_vars = wrap oldpat in
  newpat, lin_vars

let make_lin_match_case ({pc_lhs=pat;pc_rhs=expr} as case) =
  let newpat, linvars = lin_pattern pat in
  {case with pc_lhs=newpat; pc_rhs=add_putval linvars expr}

let rec linval ({pexp_desc;pexp_loc;pexp_attributes} as outer) =
  match pexp_desc with
  | Pexp_ident _ | Pexp_constant _
  | Pexp_construct (_,None)
  | Pexp_variant (_,None) ->
     outer, []

  | Pexp_apply ({pexp_desc=Pexp_ident {txt=Lident"!!"}} , [(Nolabel,exp)]) ->
     let newvar = newname "linval" in
     constr ~loc:pexp_loc "Linocaml.Base.Lin_Internal__" [longident ~loc:pexp_loc newvar], [(newvar,exp)]

  | Pexp_tuple (exprs) ->
    let exprs, bindings = List.split (List.map linval exprs) in
    {pexp_desc=Pexp_tuple(exprs);pexp_loc;pexp_attributes}, List.concat bindings

  | Pexp_construct ({txt=Lident "Data"},Some(expr)) ->
     constr ~loc:pexp_loc ~attrs:pexp_attributes "Linocaml.Base.Data_Internal__" [expr], []

  | Pexp_construct (lid,Some(expr)) ->
     let expr, binding = linval expr in
     {pexp_desc=Pexp_construct(lid,Some(expr));pexp_loc;pexp_attributes}, binding
  | Pexp_variant (lab,Some(expr)) ->
     let expr, binding = linval expr in
     {pexp_desc=Pexp_variant(lab,Some(expr));pexp_loc;pexp_attributes}, binding
  | Pexp_record (fields,expropt) ->
     let fields, bindings =
       List.split (List.map (fun (lid,expr) -> let e,b = linval expr in (lid,e),b) fields)
     in
     let bindings = List.concat bindings in
     let expropt, bindings =
       match expropt with
       | Some expr ->
          let expr, binding = linval expr in
          Some expr, binding @ bindings
       | None -> None, bindings
     in
     {pexp_desc=Pexp_record(fields,expropt);pexp_loc;pexp_attributes}, bindings
  | Pexp_array (exprs) ->
     let exprs, bindings =
       List.split (List.map linval exprs)
     in
     {pexp_desc=Pexp_array(exprs);pexp_loc;pexp_attributes}, List.concat bindings
  | Pexp_constraint (expr,typ) ->
     let expr, binding = linval expr
     in
     {pexp_desc=Pexp_constraint(expr,typ);pexp_loc;pexp_attributes}, binding
  | Pexp_coerce (expr,typopt,typ) ->
     let expr, binding = linval expr
     in
     {pexp_desc=Pexp_coerce(expr,typopt,typ);pexp_loc;pexp_attributes}, binding
  | Pexp_lazy expr ->
     let expr, binding = linval expr
     in
     {pexp_desc=Pexp_lazy(expr);pexp_loc;pexp_attributes}, binding
  | Pexp_open (oflag,lid,expr) ->
     let expr, binding = linval expr
     in
     {pexp_desc=Pexp_open(oflag,lid,expr);pexp_loc;pexp_attributes}, binding
  | Pexp_apply (expr,exprs) ->
     let expr, binding = linval expr in
     let exprs, bindings =
       List.split @@
         List.map
           (fun (lab,expr) -> let expr,binding = linval expr in (lab,expr),binding)
           exprs
     in
     begin match binding @ List.concat bindings with
     | [] -> {pexp_desc=Pexp_apply(expr,exprs);pexp_loc;pexp_attributes}, []
     | _ ->
        error pexp_loc "function call inside %linval cannot contain slot references (!! slotname)"
     end
  | Pexp_object ({pcstr_self={ppat_desc=Ppat_any}; pcstr_fields=fields} as o) ->
     let new_fields, bindings =
       List.split @@ List.map
         (function
          | ({pcf_desc=Pcf_method (name,Public,Cfk_concrete(fl,expr))} as f) ->
             let new_expr, binding = linval expr in
             {f with pcf_desc=Pcf_method(name,Public,Cfk_concrete(fl,new_expr))}, binding
          | _ ->
             error pexp_loc "object can only contain public method")
         fields
     in
     {pexp_desc=Pexp_object({o with pcstr_fields=new_fields});pexp_loc;pexp_attributes},
     List.concat bindings
  | Pexp_object _ ->
     failwith "object in linval can't refer to itself"
  | Pexp_poly (expr,None) ->
     let expr, binding = linval expr in
     {pexp_desc=Pexp_poly(expr,None);pexp_loc;pexp_attributes}, binding
  | Pexp_poly (expr,_) ->
     failwith "object method can not have type ascription"
  | Pexp_let (_,_,_) | Pexp_function _
  | Pexp_fun (_,_,_,_) | Pexp_match (_,_) | Pexp_try (_,_)
  | Pexp_field (_,_) | Pexp_setfield (_,_,_) | Pexp_ifthenelse (_,_,_)
  | Pexp_sequence (_,_) | Pexp_while (_,_) | Pexp_for (_,_,_,_,_)
  | Pexp_send (_,_) | Pexp_new _ | Pexp_setinstvar (_,_) | Pexp_override _
  | Pexp_letmodule (_,_,_) | Pexp_assert _ | Pexp_newtype (_,_)
  | Pexp_pack _ | Pexp_extension _
  | Pexp_unreachable | Pexp_letexception _
    -> failwith "%linval can only contain values"

let expression_mapper id mapper exp attrs =
  let pexp_attributes = exp.pexp_attributes @ attrs in
  let pexp_loc=exp.pexp_loc in
  let process_inner expr = mapper.Ast_mapper.expr mapper expr
  in
  match id, exp.pexp_desc with

  | "lin", Pexp_let (Nonrecursive, vbls, expr) ->
     let lin_binding ({pvb_pat} as vb) =
         let newpat, linvars = lin_pattern pvb_pat in
         {vb with pvb_pat=newpat}, linvars
     in
     let new_vbls, linvars = List.split (List.map lin_binding vbls) in
     let new_expr = add_putval (List.concat linvars) expr in
     let make_bind {pvb_pat;pvb_expr;pvb_loc;pvb_attributes} expr =
       app ~loc:pexp_loc ~attrs:pexp_attributes
           (monad_bind ())
           [pvb_expr; app ~loc:pvb_loc (mkbindfun ()) [lam ~loc:pvb_loc pvb_pat expr]]
     in
     let new_expr = List.fold_right make_bind new_vbls new_expr
     in
     Some (process_inner new_expr)

  | "lin", Pexp_match(matched, cases) ->
     let new_cases = List.map make_lin_match_case cases in
     let new_expr =
       app ~loc:pexp_loc ~attrs:pexp_attributes
         (monad_bind_raw ())
         [matched; Exp.function_ ~loc:pexp_loc new_cases]
     in
     Some (process_inner new_expr)

  | "lin", Pexp_function(cases) ->
     let cases = List.map make_lin_match_case cases in
     let new_expr =
       app ~loc:pexp_loc ~attrs:pexp_attributes
         (mkbindfun ())
         [{pexp_desc=Pexp_function(cases); pexp_loc; pexp_attributes}]
     in
     Some (process_inner new_expr)

  | "lin", Pexp_fun(Nolabel,None,pat,expr) ->
     let newpat, linvars = lin_pattern pat in
     let newexpr = add_putval linvars expr in
     let new_expr =
       app ~loc:pexp_loc ~attrs:pexp_attributes
         (mkbindfun ())
         [{pexp_desc=Pexp_fun(Nolabel,None,newpat,newexpr); pexp_loc; pexp_attributes}]
     in
     Some (process_inner new_expr)

  | "lin", _ ->
     error pexp_loc "Invalid content for extension %lin; it must be \"let%lin slotname = ..\" OR \"match%lin slotname with ..\""

  | "linret", expr ->
     let new_exp, bindings = linval {pexp_desc=expr;pexp_loc;pexp_attributes} in
     let new_exp = constr ~loc:pexp_loc "Linocaml.Base.Lin_Internal__" [new_exp] in
     let new_exp = app (monad_return_raw ()) [new_exp] in
     let new_exp = add_takeval bindings new_exp in
     Some(new_exp)

  | _ -> None

let runner ({ ptype_loc = loc } as type_decl) =
  match type_decl with
  | {ptype_name = {txt = name}; ptype_manifest = Some ({ptyp_desc = Ptyp_object (labels, Closed)})} ->
    let obj =
      let meth (fname,_,_) =
        {pcf_desc =
           Pcf_method (fname,
                       Public,
                       Cfk_concrete(Fresh, emptyslot ()));
         pcf_loc = Location.none;
         pcf_attributes = []}
      in
      constr "Linocaml.Base.Lin_Internal__" [Exp.object_ {pcstr_self = Pat.any (); pcstr_fields = List.map meth labels}]
    in
    let objtyp =
      let methtyp (fname,_,_) = (fname,[],tconstr "Linocaml.Base.empty" [])
      in
      tconstr "Linocaml.Base.lin" [Typ.object_ (List.map methtyp labels) Closed]
    in
    let mkfun = Exp.fun_ Label.nolabel None in
    let runner = mkfun (pvar "x") (mkfun (pconstr "()" []) ((app (runmonad ()) [app (evar "x") [constr "()" []]; obj])))
    and linval = disposeenv () in
    let runnertyp = Typ.arrow Nolabel (Typ.arrow Nolabel (tconstr "unit" []) (tconstr "monad" [objtyp; objtyp; Typ.any ()])) (Typ.any ())
    and linvaltyp = Typ.arrow Nolabel (tconstr "monad" [Typ.any (); objtyp; Typ.any ()]) (Typ.any ()) in
    let runner = {pstr_desc = Pstr_value (Nonrecursive, [Vb.mk (Pat.constraint_ (pvar ("run_" ^ name)) runnertyp) runner]); pstr_loc = Location.none}
    and linval = {pstr_desc = Pstr_value (Nonrecursive, [Vb.mk (Pat.constraint_ (pvar ("linval_"^name)) linvaltyp) linval]); pstr_loc = Location.none}
    in
    [runner; linval]
  | _ -> error loc "run_* can be derived only for record or closed object types"

let has_runner attrs =
  List.exists (fun ({txt = name},_) -> name = "runner")  attrs

let mapper_fun _ =
  let open Ast_mapper in
  let rec expr mapper outer =
  match outer with
  | {pexp_desc=Pexp_extension ({ txt = id }, PStr([{pstr_desc=Pstr_eval(inner,inner_attrs)}])); pexp_attributes=outer_attrs} ->
     begin match expression_mapper id mapper inner (inner_attrs @ outer_attrs) with
     | Some exp -> exp
     | None -> default_mapper.expr mapper outer
     end
  (* Lens.compose to gain full polymorphism *)
  | {pexp_desc=Pexp_apply({pexp_desc=Pexp_ident({txt=Lident "##.";loc});pexp_loc=inner_loc;pexp_attributes=inner_attr},[(_,e1);(_,e2)]); pexp_loc=outer_loc; pexp_attributes=outer_attr} ->
     let e1 = expr mapper e1
     and e2 = expr mapper e2 in
     (* brain-dead specialization of Lens.compose. problematic in various aspects: name capture, duplicated code, ... *)
     [%expr let open Linocaml.Lens in {get=(fun out1__ -> [%e e1].get ([%e e2].get out1__)); put=(fun out1__ b__ -> [%e e2].put out1__ ([%e e1].put ([%e e2].get out1__) b__))}]
  | _ -> default_mapper.expr mapper outer
  and stritem mapper outer =
    match outer with
    | {pstr_desc = Pstr_type (_,type_decls)} ->
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

let () = Ast_mapper.register "ppx_linocaml" mapper_fun
