let square x = x * x;;

(*
expr
=>let
=>let-binding
  =>value-name
    =>square
  =>parameter-list
    =>[x]
  =>'=' 
  =>expr
    =>expr
      =>object-expr
        =>lowercase-ident
          =>x
    =>infix-op
      =>'*'
    =>expr
      =>object-expr
        =>lowercase-ident
          =>x
*)