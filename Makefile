SRCDIR := src
MAINSRC := $(SRCDIR)/main.sml
EXE := $(SRCDIR)/main

all: tc

tc: $(EXE)
	@(./$(EXE))

$(EXE):
	@mlton $(MAINSRC)

docker:
	@(docker-compose up)
	@(docker-compose down)


.PHONY: clean
clean:
	@(rm src/main)

