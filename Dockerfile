FROM ubuntu

RUN apt-get update -y

RUN apt-get install smlnj rlwrap ml-yacc ml-lex ml-burg mlton make -y

WORKDIR /code
COPY . /code/
