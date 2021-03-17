EXE := tiger/tiger
TIG := $(addprefix tiger/, tiger.mlb ast.sml tiger.grm.sml tiger.lex.sml tiger.sml)
GEN := $(addprefix tiger/, *.grm.sml *.lex.sml *.grm.desc *.grm.sig)

.PHONY: all clean tests test

all: ${EXE}
	@echo "Make done Successfully, do 'make run' to execute the executable"

tc: ${EXE}
	@echo "Make done Successfully, do 'make run' to execute the executable"

run: ${EXE}
	./${EXE}

${EXE}: ${TIG}
	mlton -output $@ $<

%.lex.sml: %.lex
	mllex $<

%.grm.sml: %.grm
	mlyacc $<

clean:
	rm -f ${EXE} ${GEN}

docker:
	@docker container run -it --rm -v $(shell pwd):/code -w /code piyushkurur/compilers bash
