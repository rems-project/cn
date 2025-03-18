(** Common error types shared by TypeErrors and other modules *)

module Loc = Locations

type label_kind = Where.label

type access =
  | Load
  | Store
  | Deref
  | Kill
  | Free
  | To_bytes
  | From_bytes

type call_situation =
  | FunctionCall of Sym.t
  | LemmaApplication of Sym.t
  | LabelCall of label_kind
  | Subtyping

type situation =
  | Access of access
  | Call of call_situation

type compile_message =
  | Cannot_convert_enum_const of Z.t
  | Cannot_convert_enum_expr of unit Cerb_frontend.AilSyntax.expression
  | Cerb_frontend of Locations.t * Cerb_frontend.Errors.cause
  | Global of Global.error
  | WellTyped of WellTyped.message
  | Illtyped_binary_it of
      { left : IndexTerms.Surface.t;
        right : IndexTerms.Surface.t;
        binop : Cerb_frontend.Cn.cn_binop
      }
  | Builtins of Builtins.message
  | First_iarg_missing
  | First_iarg_not_pointer of
      { pname : Request.name;
        found_bty : BaseTypes.t
      }
  | Generic of Pp.document [@deprecated "Temporary, for refactor, to be deleted."]
