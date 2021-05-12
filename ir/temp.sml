signature TEMP =
sig
    type label
    type temp
    val newlabel : unit -> int
    val newtemp  : unit -> int
    val stackReg : int
    val wordSize : int

    (* val pushStack   : int -> int
    val popStack    : int -> int
    val getStack    : int -> int *)
end
structure Temp :> TEMP = struct

    type label = int
    type temp  = int

    val nextLabel = ref 0
    val nextTemp  = ref 0

    val stackReg = 29
    fun newlabel () = (nextLabel := !nextLabel + 1; !nextLabel)
    fun newtemp  () = (nextTemp  := !nextTemp  + 1; if (!nextTemp=stackReg) then newtemp() else !nextTemp)

    val wordSize = 64
    (* val stackAddress = ref 0
    fun pushStack n = (stackAddress := !stackAddress + (n * wordSize); !stackAddress)
    fun popStack n = (stackAddress := !stackAddress - (n * wordSize); !stackAddress)
    fun getStack n = !stackAddress *)

end