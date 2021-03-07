structure MIPS = struct

datatype regs = ...

datatype  ('l,'t) inst = add of 't * 't * 't
                        | jump of 'l

datatype ('l , 't) inst = la of 't * 'l

datatype Label = LUser of string
                | LTemp of int

(*

The code generated will be of type (Label, Temp) inst

fun toString (LUser s ) = "_" ^ s
    | toString (LTemp i)  = "_" ^ Int.toString i

*)

 (* actual code that SPIM can understand is (string, reg) inst *)
