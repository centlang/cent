SRC_DIR := src
INCLUDE_DIR := include

OBJ_DIR := obj
BIN_DIR := bin

CXX := c++
CLANG_TIDY := clang-tidy

CXX_FLAGS := -pedantic -Wall -Wextra -std=c++20 -O3 -c -I$(INCLUDE_DIR) \
    -fno-exceptions

LD_FLAGS := -lLLVM -lfmt

SRC_FILES := $(wildcard $(SRC_DIR)/**/*.cpp $(SRC_DIR)/*.cpp)
OBJ_FILES := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR)/%.o,$(SRC_FILES))

TARGET := $(BIN_DIR)/centc

PREFIX := /usr/local
DEST_BIN_DIR := $(PREFIX)/bin

all: $(TARGET)

$(TARGET): $(OBJ_FILES)
	@mkdir -p $(@D)

	$(CXX) -o $@ $^ $(LD_FLAGS)

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(@D)

	$(CXX) -o $@ $< $(CXX_FLAGS)

install: $(TARGET) | $(DEST_BIN_DIR)
	cp $(TARGET) $(DEST_BIN_DIR)

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

check: $(SRC_FILES)
	$(CLANG_TIDY) $^ -- $(CXX_FLAGS)

.PHONY: all clean install check
