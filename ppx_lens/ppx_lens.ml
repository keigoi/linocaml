open Migrate_parsetree
open Ast_405

open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience_405

module Convert_current = Migrate_parsetree_versions.Convert(OCaml_405)(OCaml_current)
module Convert_405 = Migrate_parsetree_versions.Convert(OCaml_current)(OCaml_405)

let deriver = "lens"
let raise_errorf = Ppx_deriving.raise_errorf

let getterfield () = "Linocaml.Lens.get"
let setterfield () = "Linocaml.Lens.put"


let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)

let rec traverse f ({ptyp_desc; ptyp_loc = loc; _} as typ) =
  let f ptyp_desc =
    match ptyp_desc with
    | Ptyp_var var -> Ptyp_var (f var)
    | Ptyp_alias(t, var) -> Ptyp_alias(traverse f t, f var)
    | Ptyp_any -> Ptyp_any
    | Ptyp_arrow(lab,t1,t2) -> Ptyp_arrow(lab, traverse f t1, traverse f t2)
    | Ptyp_tuple ts -> Ptyp_tuple (List.map (traverse f) ts)
    | Ptyp_constr(lab,ts) -> Ptyp_constr(lab, List.map (traverse f) ts)
    | Ptyp_object(flds,flg) -> Ptyp_object(List.map (fun (str,attr,t) -> (str,attr,traverse f t)) flds, flg)
    | Ptyp_class(name,ts) -> Ptyp_class(name, List.map (traverse f) ts)
    | Ptyp_poly(_, _) | Ptyp_variant(_,_,_)
    | Ptyp_package _ | Ptyp_extension _ ->
       raise_errorf ~loc "%s cannot handle a type in a field" deriver
  in
  {typ with ptyp_desc = f ptyp_desc}

let free_tvars typ =
  let rvars = ref [] in
  let f var = rvars := var::!rvars; var in
  ignore @@ traverse f typ;
  !rvars

let rename_tvars mapping typ =
  let f var =
    try List.assoc var mapping with Not_found -> var
  in
  traverse f typ

(* replace tvars in typ with fresh name *)
let change_tvars tvars typ =
  let mapping = ref [] in
  let rec fresh var =
    if List.exists (fun v->v=var) tvars then
      fresh (var^var)
    else
      var
  in
  let rename var =
    if List.exists (fun v->v=var) tvars then
      try
        List.assoc var !mapping
      with Not_found ->
        begin
          let newvar = fresh var in
          mapping := (var,newvar)::!mapping;
          newvar
        end
    else begin
      mapping := (var,var)::!mapping;
      var
    end
  in
  !mapping, traverse rename typ

let lens_typ rtyp ftyp =
  let getter_typ = Typ.arrow Label.nolabel rtyp ftyp
  in
  let vars = free_tvars getter_typ in
  let mapping, setter_2ndarg = change_tvars vars ftyp in
  let setter_result = rename_tvars mapping rtyp in
  Typ.constr (lid "Linocaml.Lens.t") [ftyp; setter_2ndarg; Typ.constr (lid "Linocaml.Base.lin") [rtyp]; Typ.constr (lid "Linocaml.Base.lin") [setter_result]]

let object_update obj labels fields =
  let meth (fname,_,_) =
    let expr =
      try
        List.assoc fname fields
      with Not_found ->
        Exp.send obj fname
    in
    {pcf_desc =
       Pcf_method (fname,
                   Public,
                   Cfk_concrete(Fresh,expr));
     pcf_loc = Location.none;
     pcf_attributes = []}
  in
  Exp.object_ {pcstr_self = Pat.any (); pcstr_fields = List.map meth labels}

let str_of_type ~options ({ ptype_loc = loc; _ } as type_decl) =
  parse_options options;
  let quoter = Ppx_deriving.create_quoter () in
  match type_decl with
  | {ptype_kind = Ptype_record labels; _} ->
    let mkfun = Exp.fun_ Label.nolabel None in
    let varname = Ppx_deriving.mangle_type_decl (`Prefix deriver) (Convert_current.copy_type_declaration type_decl) in
    let getter field =
      mkfun (pconstr "Linocaml.Base.Lin_Internal__" [pvar varname]) (Exp.field (evar varname) (lid field))
    and setter field =
      mkfun (pconstr "Linocaml.Base.Lin_Internal__" [pvar varname]) (mkfun (pvar field) (constr "Linocaml.Base.Lin_Internal__" [record ~over:(evar varname) [(field, (evar field))]]))
    in
    let typ = Ppx_deriving.core_type_of_type_decl (Convert_current.copy_type_declaration type_decl) in
    let lens { pld_name = { txt = name; _ }; pld_type ; _} =
      Vb.mk (Pat.constraint_ (pvar name) (lens_typ (Convert_405.copy_core_type typ) pld_type))
        @@ Convert_405.copy_expression
             (Ppx_deriving.sanitize ~quoter
                @@ Convert_current.copy_expression (record [(getterfield (),getter name); (setterfield (),setter name)]))
    in
    List.map lens labels
  | {ptype_manifest = Some ({ptyp_desc = Ptyp_object (labels, Closed); _} as typ); _} ->
    let typename = Ppx_deriving.mangle_type_decl (`Prefix deriver) (Convert_current.copy_type_declaration type_decl) in
    let fn = Exp.fun_ Label.nolabel None in
    let getter field =
      fn (pconstr "Linocaml.Base.Lin_Internal__" [pvar typename]) (Exp.send (evar typename) field)
    and setter field =
      fn (pconstr "Linocaml.Base.Lin_Internal__" [pvar typename]) (fn (pvar field.txt) (constr "Linocaml.Base.Lin_Internal__" [object_update (evar typename) labels [(field, (evar field.txt))]]))
    in
    let lens (field,_,ftyp) =
      Vb.mk (Pat.constraint_ (pvar field.txt) (lens_typ typ ftyp))
        @@ Convert_405.copy_expression
             (Ppx_deriving.sanitize ~quoter
                @@ Convert_current.copy_expression (record [(getterfield (),getter field); (setterfield (),setter field)]))
    in
    List.map lens labels
  | _ -> raise_errorf ~loc "%s can be derived only for record or closed object types" deriver

let sig_of_type ~options ({ ptype_loc = loc; _ } as type_decl) =
  parse_options options;
  match type_decl with
  | {ptype_kind = Ptype_record labels; _} ->
    let typ = Ppx_deriving.core_type_of_type_decl (Convert_current.copy_type_declaration type_decl) in
    let lens { pld_name = { txt = name; _ }; pld_type; _ } =
      Sig.value (Val.mk (Location.mknoloc name) (lens_typ (Convert_405.copy_core_type typ) pld_type))
    in
    List.map lens labels
  | {ptype_manifest = Some ({ptyp_desc = Ptyp_object (labels, Closed); _} as typ); _} ->
    let lens (field,_,ftyp) =
      Sig.value (Val.mk field (lens_typ typ ftyp))
    in
    List.map lens labels
  | _ -> raise_errorf ~loc "%s can only be derived for record types" deriver


let () =
  Ppx_deriving.(register (create deriver
    ~type_decl_str: (fun ~options ~path:_ type_decls ->
      let options = List.map (fun (str, expr) -> (str, Convert_405.copy_expression expr)) options in
      let type_decls = List.map Convert_405.copy_type_declaration type_decls in
      Convert_current.copy_structure @@ [Str.value Nonrecursive (List.concat (List.map (str_of_type ~options) type_decls))])
    ~type_decl_sig: (fun ~options ~path:_ type_decls ->
      let options = List.map (fun (str, expr) -> (str, Convert_405.copy_expression expr)) options in
      let type_decls = List.map Convert_405.copy_type_declaration type_decls in
      Convert_current.copy_signature @@
      List.concat (List.map (sig_of_type ~options) type_decls))
    ()
  ))
