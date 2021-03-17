TIG_BIN := tiger/tiger
TIG := $(addprefix tiger/, tiger.mlb ast.sml tiger.grm.sml tiger.lex.sml tiger.sml)
TIG_GEN := $(addprefix tiger/, *.grm.sml *.lex.sml *.grm.desc *.grm.sig)

.PHONY: all clean tests test

all: ${TIG_BIN}
	@echo "Make done Successfully, run executable - tiger"

tc: ${TIG_BIN}
	@echo "Make done Successfully, run executable - tiger"

${TIG_BIN}: ${TIG}
	mlton -output $@ $<

%.lex.sml: %.lex
	mllex $<

%.grm.sml: %.grm
	mlyacc $<

clean:
	rm -f ${TIG_BIN} ${TIG_GEN}

docker:
	@docker container run -it --rm -v $(shell pwd):/code -w /code piyushkurur/compilers bash
