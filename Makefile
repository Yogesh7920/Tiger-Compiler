SRCDIR := src
TIGERDIR := tiger
TARGETDIR := target

TC := $(TIGERDIR)/tc.sml
TIGERAST := $(TIGERDIR)/ast.sml
TARGETAST := $(TARGETDIR)/mips.sml

EXE := $(TIGERDIR)/tc
COMP_TIGER_AST := $(TIGERDIR)/ast
COMP_TARGET_AST := $(TARGETDIR)/mips

all: tc mips src

tc: $(COMP_TIGER_AST)
	@(./$(COMP_TIGER_AST))


mips: $(COMP_TARGET_AST)
	@(./$(COMP_TARGET_AST))

src: $(EXE)
	@(./$(EXE))

$(COMP_TIGER_AST):
	@mlton $(TIGERAST)

$(COMP_TARGET_AST):
	@mlton $(TARGETAST)

$(EXE):
	@mlton $(TC)

docker:
	@docker container run -it --rm -v $(shell pwd):/code -w /code piyushkurur/compilers bash
	
.PHONY: clean
clean:
	@(rm $(EXE) $(COMP_TIGER_AST) $(COMP_TARGET_AST) -f)

