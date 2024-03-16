let caml_ = "caml_"

type c_type = Prim of string | Struct of string | Ptr of c_type | Void
type c_prim = Int of int | Float of float | Str of string

type t =
  | C_function of {
      fn_ret : c_type;
      fn_name : string;
      fn_params : (c_type * string) list;
      fn_body : t list;
    }
  | C_call of { cc_name : string; cc_args : t list }
  | C_assign of { asg_var : t; asg_value : t }
  | C_decl of { dcl_name : string; dcl_type : c_type; dcl_value : t option }
  | C_prim of c_prim
  | C_ptr_field_access of { pfa_name : t; pfa_field : string }
  | C_field_access of { acc_name : t; acc_field : string }
  | C_variable of string
  | C_return of t
  | C_type of c_type
  | C_include of string

type program = t list

let pp_list sep pp_el fmt t =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%s" sep)
    pp_el fmt t

let rec pp fmt program = List.iter (fun (t : t) -> pp_t fmt t) program

and pp_t fmt t =
  match t with
  | C_function { fn_ret; fn_name; fn_params; fn_body } ->
      Format.fprintf fmt "%a %s(%a) {\n  %a;\n}\n\n" pp_type fn_ret fn_name
        (pp_list ", " pp_arg) fn_params (pp_list ";\n  " pp_t) fn_body
  | C_call { cc_name; cc_args } ->
      Format.fprintf fmt "%s(%a)" cc_name (pp_list ", " pp_t) cc_args
  | C_assign { asg_var; asg_value } ->
      Format.fprintf fmt "%a = %a" pp_t asg_var pp_t asg_value
  | C_decl { dcl_name; dcl_type; dcl_value = None } ->
      Format.fprintf fmt "%a %s" pp_type dcl_type dcl_name
  | C_decl { dcl_name; dcl_type; dcl_value = Some v } ->
      Format.fprintf fmt "%a %s = %a" pp_type dcl_type dcl_name pp_t v
  | C_prim prim -> pp_prim fmt prim
  | C_ptr_field_access { pfa_name; pfa_field } ->
      Format.fprintf fmt "%a->%s" pp_t pfa_name pfa_field
  | C_field_access { acc_name; acc_field } ->
      Format.fprintf fmt "%a.%s" pp_t acc_name acc_field
  | C_variable s -> Format.fprintf fmt "%s" s
  | C_return t -> Format.fprintf fmt "return %a" pp_t t
  | C_type t -> Format.fprintf fmt "%a" pp_type t
  | C_include s -> Format.fprintf fmt "#include %s\n" s

and pp_arg fmt (ctype, name) = Format.fprintf fmt "%a %s" pp_type ctype name

and pp_type fmt (ctype : c_type) =
  match ctype with
  | Prim x -> Format.fprintf fmt "%s" x
  | Struct s -> Format.fprintf fmt "struct %s" s
  | Ptr t -> Format.fprintf fmt "%a*" pp_type t
  | Void -> Format.fprintf fmt "void"

and pp_prim fmt (prim : c_prim) =
  match prim with
  | Int d -> Format.fprintf fmt "%d" d
  | Float f -> Format.fprintf fmt "%f" f
  | Str s -> Format.fprintf fmt "%s" s

let decl dcl_type dcl_name dcl_value = C_decl { dcl_name; dcl_type; dcl_value }
let call cc_name cc_args = C_call { cc_name; cc_args }
let var x = C_variable x
let assign asg_var asg_value = C_assign { asg_var; asg_value }
let int x = C_prim (Int x)
let float f = C_prim (Float f)
let string x = C_prim (Str x)
let ptr_field pfa_name pfa_field = C_ptr_field_access { pfa_name; pfa_field }
let typ t = C_type t
let field acc_name acc_field = C_field_access { acc_name; acc_field }
let return x = C_return x

(* caml functions *)
let caml_params x = call ("CAMLparam" ^ Int.to_string (List.length x)) x
let caml_locals x = call ("CAMLlocal" ^ Int.to_string (List.length x)) x
let caml_return0 = var "CAMLreturn0"
let caml_return x = call "CAMLreturn" [ x ]

let caml_alloc_tuple fields =
  call "caml_alloc_tuple" [ int (List.length fields) ]

let store_field var idx value = call "Store_field" [ var; int idx; value ]
let val_int var name = call "Val_int" [ ptr_field var name ]
let int_val var idx = call "Int_val" [ call "Field" [ var; int idx ] ]

(* Unlike Int, Float must be re-boxed before returning to the OCaml runtime thus a copy*)
let val_float var name = call "caml_copy_double" [ ptr_field var name ]
let float_val var idx = call "Double_val" [ call "Field" [ var; int idx ] ]

(* from_ir *)
let rec ctype_of_ir (ir_type : Ir.ir_type) =
  match ir_type with
  | Ir.Abstract s -> Prim s
  | Ir.Record { rec_name; _ } -> Prim rec_name
  | Ir.Enum { enum_name; _ } -> Prim enum_name
  | Ir.Prim Ir.Int -> Prim "int"
  | Ir.Prim Ir.Float -> Prim "float"
  | Ir.Prim Ir.Bool -> Prim "bool"
  | Ir.Prim Ir.Char -> Prim "char"
  | Ir.Prim Ir.Void -> Void
  | Ir.Ptr t -> Ptr (ctype_of_ir t)
  | Ir.Func _ -> assert false

let rec ctype_name typ =
  match typ with
  | Prim n -> n
  | Struct n -> n
  | Void -> "void"
  | Ptr t -> ctype_name t

module Shims = struct
  let to_value name fields =
    C_function
      {
        fn_ret = Prim "value";
        fn_name = caml_ ^ name ^ "_to_value";
        fn_params = [ (Ptr (Struct name), "x") ];
        fn_body =
          [
            caml_params [];
            caml_locals [ var "caml_x" ];
            assign (var "caml_x") (caml_alloc_tuple fields);
          ]
          @ Ir.(
              List.mapi
                (fun idx fld ->
                  let fld_caml_value =
                    match fld.fld_type with
                    | Prim Float -> val_float (var "x") fld.fld_name
                    | _ -> val_int (var "x") fld.fld_name
                  in
                  store_field (var "caml_x") idx fld_caml_value)
                fields)
          @ [ caml_return (var "caml_x") ];
      }

  let of_value name fields =
    C_function
      {
        fn_ret = Ptr (Prim name);
        fn_name = caml_ ^ name ^ "_of_value";
        fn_params = [ (Prim "value", "caml_x") ];
        fn_body =
          [
            decl (Ptr (Prim name)) "x"
              (Some (call "malloc" [ call "sizeof" [ typ (Struct name) ] ]));
          ]
          @ Ir.(
              List.mapi
                (fun idx fld ->
                  let field_c_value =
                    match fld.fld_type with
                    | Prim Float -> float_val (var "caml_x") idx
                    | _ -> int_val (var "caml_x") idx
                  in
                  assign (ptr_field (var "x") fld.fld_name) field_c_value)
                fields)
          @ [ return (var "x") ];
      }

  let wrap_fun Ir.{ fndcl_name; fndcl_type } =
    match fndcl_type with
    | Func { fn_ret; fn_params } ->
        let fn_ret = ctype_of_ir fn_ret in

        let fn_body =
          let declare_params =
            [
              caml_params
                (List.map (fun (name, _type) -> var (caml_ ^ name)) fn_params);
            ]
          in
          let maybe_declare_result =
            match fn_ret with
            | Void -> []
            | _ -> [ caml_locals [ var "result" ] ]
          in

          let transform_values =
            List.map
              (fun (name, param_type) ->
                let c_type = ctype_of_ir param_type in
                match c_type with
                | Ptr _t ->
                    let _c_type_name = ctype_name c_type in
                    decl c_type name
                      (Some
                         (call "Nativeint_val"
                            [ call "Field" [ var (caml_ ^ name); int 1 ] ]))
                | _ ->
                    let c_type_name = ctype_name c_type in
                    decl c_type name
                      (Some
                         (call
                            (caml_ ^ c_type_name ^ "_of_value")
                            [ var (caml_ ^ name) ])))
              fn_params
          in

          let call_and_return =
            let fn_call =
              call fndcl_name (List.map (fun (param, _) -> var param) fn_params)
            in
            match fn_ret with
            | Void -> [ fn_call; caml_return0 ]
            | _ -> [ assign (var "result") fn_call; caml_return (var "result") ]
          in

          List.flatten
            [
              declare_params;
              maybe_declare_result;
              transform_values;
              call_and_return;
            ]
        in

        C_function
          {
            fn_ret;
            fn_params =
              List.map
                (fun (name, _type) -> (Prim "value", caml_ ^ name))
                fn_params;
            fn_name = caml_ ^ fndcl_name;
            fn_body;
          }
    | _ -> assert false
end

let from_ir (ir : Ir.t) : program =
  [
    C_include (Format.sprintf "%S" ir.header);
    C_include "<caml/alloc.h>";
    C_include "<caml/callback.h>";
    C_include "<caml/fail.h>";
    C_include "<caml/memory.h>";
    C_include "<caml/mlvalues.h>";
    C_include "<caml/unixsupport.h>";
  ]
  @ (List.filter_map
       (fun node ->
         match node with
         | Ir.Ir_fun_decl fun_decl -> Some [ Shims.wrap_fun fun_decl ]
         | Ir.Ir_type (Record { rec_name; rec_fields }) ->
             Some
               [
                 Shims.of_value rec_name rec_fields;
                 Shims.to_value rec_name rec_fields;
               ]
         | _ -> None)
       ir.items
    |> List.flatten)
