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
                            ADDI of 't * 't * int   |
                            ADDU of 't * 't * 't    |
                            ADDIU of 't * 't * int  |
                            SUB of 't * 't * 't     |
                            SUBU of 't * 't * 't    |
                            MUL of 't * 't * 't     |
                            MULO of 't * 't * 't    |
                            MULOU of 't * 't * 't   |
                            MULT of 't * 't         |
                            MULTU of 't * 't        |

                            AND of 't * 't * 't     |
                            ANDI of 't * 't * int   |
                            OR of 't * 't * 't      |
                            ORI of 't * 't * int    |
                            NEG of 't * 't          |
                            NEGU of 't * 't * 't    |
                            NOT of 't * 't          |
                            XOR of 't * 't * 't     |
                            XORI of 't * 't * int   |
                            REM of 't * 't * 't     |
                            REMU of 't * 't * 't    |
                            ROL of 't * 't * 't     |
                            ROR of 't * 't * 't     |

                            SLL of 't * 't * int    |
                            SLLV of 't * 't * 't    |
                            SRA of 't * 't * int    |
                            SRAV of 't * 't * 't    |
                            SRL  of 't * 't * int   |
                            SRLV of 't * 't * 't    |

                            LW of 't * 't * int     |
                            SW of 't * 't * int     |
                            LUI of 't * int         |
                            LA of 't * 'l           |
                            LI of 't * int          |

                            MOVE of 't * 't         |
                            MFHI of 't              |
                            MFLO of 't              |
                            MTHI of 't              |
                            MTLO of 't              |

                            SEQ of 't * 't * 't     |
                            SGE of 't * 't * 't     |
                            SGEU of 't * 't * 't    |
                            SGT of 't * 't * 't     |
                            SGTU of 't * 't * 't    |
                            SLE of 't * 't * 't     |
                            SLEU of 't * 't * 't    |
                            SLT of 't * 't * 't     |
                            SLTI of 't * 't * int   |
                            SLTU of 't * 't * 't    |
                            SLTUI of 't * 't * int  |
                            SNE of 't * 't * 't     |

                            B of 'l                 |
                            BCZT of 'l              |
                            BCZF of 'l              |
                            BEQ of 't * 't * 'l     |
                            BEQZ of 't * 't * 'l    |
                            BNE of 't * 't * 'l     |
                            BGT of 't * 't * 'l     |
                            BGTU of 't * 't * 'l    |
                            BGTZ of 't * 't * 'l    |
                            BGE of 't * 't * 'l     |
                            BGEU of 't * 't * 'l    |
                            BGEZ of 't * 'l         |
                            BLT of 't * 't * 'l     |
                            BLTU of 't * 't * 'l    |
                            BLTZ of 't * 't * 'l    |
                            BLE of 't * 't * 'l     |

                            J of 'l                 |
                            JR of 't                |
                            JAL of 'l               |
                            JALR of 't              |
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
