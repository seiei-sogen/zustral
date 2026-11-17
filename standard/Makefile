MODULES := \
		src/Tuples.zui,src/Tuples.zum \
		src/Bounded.zui,src/Bounded.zum \
		src/Equality.zui,src/Equality.zum \
		src/Order.zui,src/Order.zum \
		src/Box.zui,src/Box.zum \
		src/Buffer.zui,src/Buffer.zum \
		src/String.zui,src/String.zum \
		src/StringBuilder.zui,src/StringBuilder.zum \
		src/IO/IO.zui,src/IO/IO.zum \
		src/IO/Terminal.zui,src/IO/Terminal.zum \
		test/Unit.zui,test/Unit.zum \
		test/Tuples.zui,test/Tuples.zum \
		test/Bounded.zui,test/Bounded.zum \
		test/Equality.zui,test/Equality.zum \
		test/Order.zui,test/Order.zum \
		test/Box.zui,test/Box.zum \
		test/Buffer.zui,test/Buffer.zum \
		test/String.zui,test/String.zum \
		test/StringBuilder.zui,test/StringBuilder.zum \
		test/IO/Terminal.zui,test/IO/Terminal.zum \
		test/Main.zui,test/Main.zum
TEST_BIN := test_bin

.DEFAULT: all
all: $(TEST_BIN)

$(TEST_BIN): src/*.zui src/*.zum src/*/*.zui src/*/*.zum test/*.zui test/*.zum test/*/*.zui test/*/*.zum
	../austral compile $(MODULES) --entrypoint=Standard.Test:Main --output=$(TEST_BIN)

.PHONY: clean
clean:
	-test -f $(TEST_BIN) && rm $(TEST_BIN)
