signature TEMP =
sig
    type label
    type temp
    val newlabel : unit -> int
    val newtemp  : unit -> int
end
structure Temp :> TEMP = struct

    type label = int
    type temp  = int

    val nextLabel = ref 0
    val nextTemp  = ref 0

    fun newlabel () = (nextLabel := !nextLabel + 1; !nextLabel)
    fun newtemp  () = (nextTemp  := !nextTemp  + 1; !nextTemp)

end