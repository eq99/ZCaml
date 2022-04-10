1 + 2 * 3;;

(*
expr
=>expr
  =>constant
    =>integer-literal
      =>1
=>infix-op
  =>+
=>expr
  =>expr
    =>constant
      =>integer-literal
        =>2
  =>infix-op
    =>*
  =>expr
    =>constant
      =>integer-literal
        =>3
*)