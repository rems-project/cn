SRC_DIR := ./src

SRCS := $(shell find $(SRC_DIR) -name '*.c')
TESTS := $(SRCS:%=testing-%)

.PHONY: test
test: $(TESTS)

$(TESTS): testing-%: %
	./run-single-test.sh $<
