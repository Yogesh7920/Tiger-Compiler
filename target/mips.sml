structure MIPS = struct

datatype regs = zero | at | v0 | v1 | (* $0 - $3 *)
                a0   | a1 | a2 | a3 | (* $4 - $7 *)
                t0   | t1 | t2 | t3 | t4 | t5 | t6 | t7 | (* $8 - $15 *)
                s0   | s1 | s2 | s3 | s4 | s5 | s6 | s7 | (* $16 - $23 *)
                t8   | t9 | (* $24 - $25 *)
                k0   | k1 | (* $26 - $27 *)
                gp   | sp | fp | ra | (* address regs *)
                f0   | f1 | f2 | f3 | (* floating return regs *)
                f4   | f5 | f6 | f7 | f8 | f9 | f10 (* temp. regs *)

datatype  ('l,'t) inst =    ADD of 't * 't * 't     |
                            SUB of 't * 't * 't     |
                            ADDI of 't * 't * int   |
                            MUL of 't * 't * 't     |
                            AND of 't * 't * 't     |
                            OR of 't * 't * 't      |
                            ANDI of 't * 't * int   |
                            ORI of 't * 't * int    |
                            SLL of 't * 't * int    |
                            SRL  of 't * 't * int   |
                            LW of 't * 't * int     |
                            SW of 't * 't * int     |
                            LUI of 't * int         |
                            LA of 't * 'l           |
                            LI of 't * int          |
                            MOVE of 't * 't         |
                            BEQ of 't * 't * int    |
                            BNE of 't * 't * int    |
                            BGT of 't * 't * int    |
                            BGE of 't * 't * int    |
                            BLT of 't * 't * int    |
                            BLE of 't * 't * int    |
                            SLT of 't * 't * 't     |
                            SLTI of 't * 't * int   |
                            J of 'l                 |
                            JR of 't                |
                            JAL of 'l               |
                            print_int of int        |
                            print_float of real     |
                            print_double of real    |
                            print_string of string  |
                            read_int of int         |
                            read_float of real      |
                            read_double of real     |
                            read_string of string   |
                            exit                    |
                            exit2


datatype Label = LUser of string
                | LTemp of int

(*

The code generated will be of type (Label, Temp) inst

fun toString (LUser s ) = "_" ^ s
    | toString (LTemp i)  = "_" ^ Int.toString i

*)

 (* actual code that SPIM can understand is (string, reg) inst *)
