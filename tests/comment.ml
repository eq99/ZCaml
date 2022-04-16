
(**Requires: [n >= 0].*) 1+1 (*[odd n] is whether [n] is odd.(*ooxx**)**) +1;;



(** [even n] is whether [n] is even.
    Requires: [n >= 0]. *)
let rec even n =
  n = 0 || odd (n - 1)

(** [odd n] is whether [n] is odd.
    Requires: [n >= 0]. *)
and odd n =
  n <> 0 && even (n - 1);;