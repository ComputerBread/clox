# Define the compiler
CC = gcc

# Define compiler flags
CFLAGS = -Wall -Wextra -std=c99 #-Werror 

# Define the build directory
BUILD_DIR = build

# Define the executable name with the build directory
TARGET = $(BUILD_DIR)/main

# Automatically find all .c files in the directory
SRCS = $(wildcard *.c)

# Generate a list of .o files in the build directory
OBJS = $(SRCS:%.c=$(BUILD_DIR)/%.o)

# Default rule to create the executable
all: $(TARGET)

# Rule to build the executable
$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS)

# Rule to compile .c files to .o files in the build directory
$(BUILD_DIR)/%.o: %.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Create the build directory if it doesn't exist
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Define the clean rule to remove generated files and the build directory
.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)

# Define the run rule to compile and run the program
.PHONY: run
run: $(TARGET)
	./$(TARGET)
