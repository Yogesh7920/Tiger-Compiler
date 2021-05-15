# Git hashes and run info

Lab 6

    How to run: 
    There are five ways to execute
    1 './tc filepath' , this prints the IR
    2. './tc --ir filepath' , this also prints the IR
    3. './tc --can filepath' , this prints the canonized IR
    4. './tc --pp filepath' , this pretty prints the tiger code
    5. './tc --ast filepath' , this prints the AST

Lab 5
    
    How to run: no output, check tree.sml, temp.sml and canon.sml
    hash: bc973183548e1137373bfea74dc1fffd6646bf2c
    
Lab 4
    
    efa79880cbbe92b8c4987919ffb829cadc248496

Lab 3

    8c5496bc044730d0a331c37e51cddb230eafa324

# Change Log 

  _(date: yyyy-mm-dd)_

### 2021-05-14
- Tree to IR done

### 2021-05-12
- Tree to IR basic done (without functions)
### 2021-05-01
- Tiger to Tree, basic binary added
### 2021-04-30
- Printing IR added
- Basic AST -> IR added.
### 2021-04-27
- Canonization done. 
### 2021-04-21
- Commandline arguments added for tiger executable
### 2021-04-19
- The basic requirements of tree.sml and temp.sml done
- The code has been refactored as per the instructions.pdf
### 2021-04-11
- The intermediate representation basic added.

### 2021-03-29
- Logic for indentation added.
- Color code added for different words.
### 2021-03-22
- 51 Test cases added
- Printing AST Done
- Multiple test case added
### 2021-03-16
- Tiger Complied successfully.

### 2021-03-15

- Tiger lexer done (tiger/tiger.lex)
- Tiger parser done (tiger/tiger.grm) 
### 2021-03-14
#### Added
- Tiger Parser Added
- Tiger Lexer Added
### 2021-03-08
- Tiger AST captured.
- Many MIPS instruction captured.
- Tiger AST Test capture done.
### 2021-03-07
- MIPS instruction set captured.
### 2021-03-06
- Basic AST for tiger done.

### 2021-03-01
- Extend the reverse polish compiler to support brackets.
### 2021-02-28
- Extended the reverse polish compiler to support division
### 2021-02-25
#### Added
- reverse-polish folder copied from <br>
  https://gitlab.com/piyush-kurur/compilers/-/tree/master/reverse-polish
### 2021-02-19

#### Added
- src folder
- main.sml inside src folder

#### Changes
- Added the following make targets
  - all
  - tc
  - docker
  - clean

### 2021-02-18
#### Added
- make
- Dockerfile
- docker-compose.yaml
#### Changes
- added target _tc_ to make; prints _hello world_.
- Dockerfile make to create the required container.
- docker-compose.yaml: _main_ service created for ease of development.
### 2021-02-17
    Initialized the project in local and gitlab.