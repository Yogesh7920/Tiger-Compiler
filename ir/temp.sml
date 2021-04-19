signature TEMP =
sig
    type label
    type temp
    val newlabel : unit -> label  (* generate a new label *)
    val newtemp  : unit -> temp
end
structure Temp :> TEMP = struct


    type label = int (* 2⁶⁴ = ∞ many variables *)
    type temp  = int


    val nextLabel = ref 0
    val nextTemp  = ref 0

    fun newlabel () = nextLabel := nextLabel + 1
    fun newtemp  () = nextTemp  := nextTemp  + 1
    
end