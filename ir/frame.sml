structure Frame =
struct
    val wordSize = 4 (* 4 bytes, 32 bits *)
    val stackptr = 29
    val frameptr = 30
    val retadrs = 31
    val reserved = [stackptr, frameptr, retadrs]
end