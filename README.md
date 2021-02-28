# General Info

    Name: Yogesh
    Roll Number: 111801047

# Lab Assignments

## Lab 1: Updating the reverse polish compiler

### Experiment-3
    Change the expr.grm file by commenting out the lines %left (The comment syntax is the same as SML)

    The complier completely ignored the order of precedence and complied with the operator on the righ as high precedence.

    E.g.:
        input: 2 + 3 * 4 + 5
        ouput: 5 4 + 3 * 2 + p

### Experiment-4
    What happens if you interchange the %left PLUS MINUS like with the %left MUL line ?

    The complier gave more precedence to plus and minus over multiply

    E.g.: 
        input: 2 + 3 * 4 + 5
        output: 5 4 + 3 2 + * p

# How to run
## There are two was to run the program
### 1. using make
    requirements: smlnj, ml-yacc, ml-lex, ml-burg, mlton, make
### 3. make docker 
    opens bash with all the requirements installed.
    required image: piyushkurur/compilers (319 MB)
    
