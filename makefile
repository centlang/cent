SRC_DIR := src
INCLUDE_DIR := include
TEST_DIR := test

OBJ_DIR := obj
BIN_DIR := bin
TEST_BIN_DIR := $(BIN_DIR)/test
TEST_OBJ_DIR := $(OBJ_DIR)/test

CXX := c++
CLANG_TIDY := clang-tidy

CXXFLAGS := -pedantic -Wall -Wextra -std=c++20 -O3 -c -I$(INCLUDE_DIR) \
    -fno-exceptions -MMD

LDFLAGS := -lLLVM -lfmt

SRC_FILES := $(shell find $(SRC_DIR) -name '*.cpp')
OBJ_FILES := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR)/%.o,$(SRC_FILES))
DEP_FILES := $(patsubst %.o,%.d,$(OBJ_FILES))
TEST_FILES := $(shell find $(TEST_DIR) -name '*.cn')
TEST_BIN_FILES := $(patsubst $(TEST_DIR)/%.cn,$(TEST_BIN_DIR)/%,$(TEST_FILES))

TARGET := $(BIN_DIR)/centc

PREFIX := /usr/local
DEST_BIN_DIR := $(PREFIX)/bin

all: $(TARGET)

$(TARGET): $(OBJ_FILES)
	@mkdir -p $(@D)

	$(CXX) -o $@ $^ $(LDFLAGS)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(@D)

	$(CXX) -o $@ $< $(CXXFLAGS)

install: $(TARGET) | $(DEST_BIN_DIR)
	cp $(TARGET) $(DEST_BIN_DIR)

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

check: $(SRC_FILES)
	$(CLANG_TIDY) $^ -- $(CXXFLAGS)

$(TEST_BIN_DIR)/%: $(TEST_DIR)/%.cn $(TARGET)
	@mkdir -p $(@D)

	$(TARGET) -o $@ $<

test: $(TEST_BIN_FILES)
	@for test in $^; do \
		echo "Running test: $$test"; \
		./$$test > /dev/null || exit 1; \
	done

-include $(DEP_FILES)

.PHONY: all install clean check test
