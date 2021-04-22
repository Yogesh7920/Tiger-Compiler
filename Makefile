EXE := tc
MAIN := $(addprefix ./, tc.mlb tc.sml)
TIG := $(addprefix tiger/, ast.sml tiger.grm.sml tiger.lex.sml)
TAR := $(addprefix target/, pp.sml)
GEN := $(addprefix tiger/, *.grm.sml *.lex.sml *.grm.desc *.grm.sig)

.PHONY: all clean tests test

all: ${EXE}
	@echo "Make done Successfully, do 'make run' to execute the executable"

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
