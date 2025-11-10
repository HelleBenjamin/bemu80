CC := gcc
CFLAGS := -O0 -g -Wall -fsanitize=address

.PHONY: all

all:
	$(CC) $(CFLAGS) -o bemu80 BEMU80.C

test:
# or just z80asm
	z88dk-z80asm -mz80 test.asm -b

disasm:
	z88dk-dis -mz80 -o 0x0000 test.bin > testdis.asm