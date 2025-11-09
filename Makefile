CC := gcc
CFLAGS := -O2

.PHONY: all

all:
	$(CC) $(CFLAGS) -o bemu80 BEMU80.C