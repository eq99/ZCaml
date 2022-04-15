let a =1 and b = 2;;


(** [even n] is whether [n] is even. Requires: [n >= 0]. *) let rec even n = n = 0 || odd (n - 1)  and (** [odd n] is whether [n] is odd.(***) Requires: [n >= 0]. *) odd n = n <> 0 && even (n - 1);;