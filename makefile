SRC_DIR := src
INCLUDE_DIR := include

OBJ_DIR := obj
BIN_DIR := bin

CXX := c++
CLANG_TIDY := clang-tidy

CXXFLAGS := -pedantic -Wall -Wextra -std=c++20 -O3 -c -I$(INCLUDE_DIR) \
    -fno-exceptions

LDFLAGS := -lLLVM -lfmt

SRC_FILES := $(wildcard $(SRC_DIR)/**/*.cpp $(SRC_DIR)/*.cpp)
OBJ_FILES := $(patsubst $(SRC_DIR)/%.cpp,$(OBJ_DIR)/%.o,$(SRC_FILES))

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

.PHONY: all clean install check
