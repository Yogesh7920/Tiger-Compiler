SRCDIR := src
MAINSRC := $(SRCDIR)/main.sml
EXE := $(SRCDIR)/main

all: tc

tc: $(EXE)
	@(./$(EXE))

$(EXE):
	@mlton $(MAINSRC)

docker:
	docker container run -it --rm -v $(shell pwd):/code piyushkurur/compilers bash
	
.PHONY: clean
clean:
	@(rm src/main)

