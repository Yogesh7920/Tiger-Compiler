# General Info

    Name: Yogesh
    Roll Number: 111801047

# How to run

## There are three was to run the program
### 1. make tc 
    requirements: smlnj, ml-yacc, ml-lex, ml-burg, mlton, make
### 2. docker-compose up
    requirements: docker, docker-compose, internet

### 3. make docker 
    similiar to the above method, also removes cleans and removes the container

# Make Targets
### 1. all: same as _make tc_
### 2. tc: compiles and executes main.sml
### 3. docker: does _make tc && make clean_ in a container and the removes it. 
