(* Abstract syntax *)

(* Variable *)
type name = string

type term =
  | LVar of name
  | LAbs of name * term
  | LApp of term * term
  | LLet of name * term * term

let string_of_term t =
  let rec to_str pr t =
    let (pr', str) = match t with
      | LVar v -> (4, v)
      | LAbs (v, b) -> (1, "fun " ^ v ^ " -> " ^ (to_str 0 b))
      | LApp (t, t') -> (3, (to_str 3 t) ^ " " ^ (to_str 3 t'))
      | LLet (v, t, b) -> (2, "let " ^ v ^ " = " ^ (to_str 0 t) ^
                              " in " ^ (to_str 0 b))
    in
    if pr' > pr then str else "(" ^ str ^ ")"
  in
  to_str (-1) t
