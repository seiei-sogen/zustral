STDLIB := \
		../../standard/src/Tuples.zui,../../standard/src/Tuples.zum \
		../../standard/src/Bounded.zui,../../standard/src/Bounded.zum \
		../../standard/src/Equality.zui,../../standard/src/Equality.zum \
		../../standard/src/Order.zui,../../standard/src/Order.zum \
		../../standard/src/Box.zui,../../standard/src/Box.zum \
		../../standard/src/Buffer.zui,../../standard/src/Buffer.zum \
		../../standard/src/String.zui,../../standard/src/String.zum \
		../../standard/src/StringBuilder.zui,../../standard/src/StringBuilder.zum \
		../../standard/src/IO/IO.zui,../../standard/src/IO/IO.zum \
		../../standard/src/IO/Terminal.zui,../../standard/src/IO/Terminal.zum \
		
PROG := FFI

ENTRY := Example.$(PROG):main

SRC := \
	$(PROG).zui,$(PROG).zum

.DEFAULT: all
all: 
	../../zustral compile $(STDLIB) $(SRC) --entrypoint=$(ENTRY) --output=main

.PHONY: clean
clean:
	rm main
