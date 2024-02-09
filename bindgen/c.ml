type c_type = Prim of string | Struct of string | Ptr of c_type
type c_prim = Int of int | Str of string

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

type program = t list

let pp_list sep pp_el fmt t =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%s" sep)
    pp_el fmt t

let rec pp fmt program = List.iter (fun (t : t) -> pp_t fmt t) program

and pp_t fmt t =
  match t with
  | C_function { fn_ret; fn_name; fn_params; fn_body } ->
      Format.fprintf fmt "%a %s(%a) {\n  %a\n}\n\n" pp_type fn_ret fn_name
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

and pp_arg fmt (ctype, name) = Format.fprintf fmt "%a %s" pp_type ctype name

and pp_type fmt (ctype : c_type) =
  match ctype with
  | Prim x -> Format.fprintf fmt "%s" x
  | Struct s -> Format.fprintf fmt "struct %s" s
  | Ptr t -> Format.fprintf fmt "%a*" pp_type t

and pp_prim fmt (prim : c_prim) =
  match prim with
  | Int d -> Format.fprintf fmt "%d" d
  | Str s -> Format.fprintf fmt "%s" s

let decl dcl_type dcl_name dcl_value = C_decl { dcl_name; dcl_type; dcl_value }
let call cc_name cc_args = C_call { cc_name; cc_args }
let var x = C_variable x
let assign asg_var asg_value = C_assign { asg_var; asg_value }
let int x = C_prim (Int x)
let string x = C_prim (Str x)
let ptr_field pfa_name pfa_field = C_ptr_field_access { pfa_name; pfa_field }
let typ t = C_type t
let field acc_name acc_field = C_field_access { acc_name; acc_field }

module Shims = struct
  let to_value name fields =
    C_function
      {
        fn_ret = Prim "value";
        fn_name = "caml_" ^ name ^ "_to_value";
        fn_params = [ (Ptr (Struct name), "x") ];
        fn_body =
          [
            call "CAMLparam0" [];
            call "CAMLlocal1" [ var "caml_x" ];
            assign (var "caml_x")
              (call "caml_alloc_tuple" [ int (List.length fields) ]);
          ]
          @ Ir.(
              List.mapi
                (fun idx field ->
                  call "Store_field"
                    [
                      var "caml_x";
                      int idx;
                      call "Val_int" [ ptr_field (var "x") field.fld_name ];
                    ])
                fields)
          @ [ call "CAMLreturn" [ var "caml_x" ] ];
      }

  let of_value name fields =
    C_function
      {
        fn_ret = Ptr (Prim name);
        fn_name = "caml_" ^ name ^ "_of_value";
        fn_params = [ (Ptr (Struct "value"), "caml_x") ];
        fn_body =
          [
            decl (Ptr (Prim name)) "x"
              (Some (call "malloc" [ call "sizeof" [ typ (Struct name) ] ]));
          ]
          @ Ir.(
              List.mapi
                (fun idx fld ->
                  assign
                    (field (var "x") fld.fld_name)
                    (call "Val_int" [ call "Field" [ int idx; var "caml_x" ] ]))
                fields)
          @ [ call "CAMLreturn" [ var "caml_x" ] ];
      }
end

let from_ir (ir : Ir.t) : program =
  List.filter_map
    (fun node ->
      match node with
      | Ir.Ir_type (Record { rec_name; rec_fields }) ->
          Some
            [
              Shims.of_value rec_name rec_fields;
              Shims.to_value rec_name rec_fields;
            ]
      | _ -> None)
    ir.items
  |> List.flatten
