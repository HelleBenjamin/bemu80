CC := gcc
# Release flags
CFLAGS := -O2 -Wall
# Debug flags
#CFLAGS := -O0 -g -Wall -fsanitize=address

# z80asm assembler
ASM := z80asm
ASMFLAGS := 

# z88dk assembler
#ASM := z88dk-z80asm
#ASMFLAGS := -mz80 -b

.PHONY: all

all:
	$(CC) $(CFLAGS) -o bemu80 BEMU80.C

test:
# or just z80asm
	$(ASM) $(ASMFLAGS) test.asm

disasm:
	z88dk-dis -mz80 -o 0x0000 test.bin > testdis.asm