EXE := tc
MAIN := $(addprefix ./, tc.mlb tc.sml)
TIG := $(addprefix tiger/, ast.sml tiger.grm.sml tiger.lex.sml)
TAR := $(addprefix target/, pp.sml)
GEN := $(addprefix tiger/, *.grm.sml *.lex.sml *.grm.desc *.grm.sig)

.PHONY: all clean tests test

all: ${EXE}
	@echo "Make done Successfully, \n \
	There are five ways to execute \n \
	\t 1. \033[;36m ./tc filepath \033[0m , this prints the IR \n \
	\t 2. \033[;36m ./tc --ir filepath \033[0m, this also prints the IR \n \
	\t 3. \033[;36m ./tc --can filepath \033[0m, this prints the canonized IR \n \
	\t 3. \033[;36m ./tc --bb filepath \033[0m, this prints the canonized upto basic-blocks of IR \n \
	\t 4. \033[;36m ./tc --pp filepath \033[0m, this pretty prints the tiger code'\n \
	\t 5. \033[;36m ./tc --ast filepath \033[0m, this prints the AST \n\n"

run: ${EXE}
	./${EXE}

${EXE}: ${MAIN} ${TIG} ${TAR}
	mlton -output $@ $<

%.lex.sml: %.lex
	mllex $<

%.grm.sml: %.grm
	mlyacc $<

test%: $(EXE)
	@echo "\n$@.tig\n"
	@./$(EXE) tests/$@.tig

tests: $(EXE)
	@chmod +x .tests.sh
	@./.tests.sh

clean:
	rm -f ${EXE} ${GEN}

docker:
	@docker container run -it --rm -v $(shell pwd):/code -w /code yogesh7920/compilers_sml bash
