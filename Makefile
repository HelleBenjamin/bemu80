CC := gcc
# Release flags
CFLAGS := -O2 -Wall -lpthread
# Debug flags
#CFLAGS := -O0 -g -Wall -fsanitize=address -lpthread

# z80asm assembler
#ASM := z80asm
#ASMFLAGS := -o test.bin

# z88dk assembler
ASM := z88dk-z80asm
ASMFLAGS := -mz80 -b

.PHONY: all

all:
	$(CC) $(CFLAGS) -o bemu80 BEMU80.C

install:
	sudo cp bemu80 /usr/local/bin

test:
# or just z80asm
	$(ASM) $(ASMFLAGS) test.asm

testrun: test
	./bemu80 test.bin

disasm:
	z88dk-dis -mz80 -o 0x0000 test.bin > testdis.asm

# EXAMPLES
example_folder := examples

example_io:
	$(ASM) $(ASMFLAGS) $(example_folder)/io.asm
