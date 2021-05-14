structure Frame =
struct
    val wordSize = 64
    val stackptr = 29
    val frameptr = 30
    val retadrs = 31
    val reserved = [stackptr, frameptr, retadrs]
end