FROM ubuntu

RUN apt-get update -y \ 
    && apt-get install smlnj rlwrap ml-yacc ml-lex ml-burg mlton make -y \
    && rm -rf /var/lib/apt/lists/*
    
WORKDIR /code

# COPY . /code/
