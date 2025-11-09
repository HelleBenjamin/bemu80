/* SPDX-License-Identifier: GPL-2.0-or-later
/Copyright (c) 2025 Benjamin Helle
*/ 
#ifndef BEMU80_H
#define BEMU80_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <sys/types.h>
#include <unistd.h>

#define MEM_SIZE 0xFFFF // 64k
#define STD_PORT 0x81

#define FLAG_C 0x01
#define FLAG_N 0x02
#define FLAG_P 0x04
#define FLAG_H 0x20
#define FLAG_Z 0x40
#define FLAG_S 0x80

#define ALU_OP_ADD 0x00
#define ALU_OP_ADC 0x01
#define ALU_OP_SUB 0x02
#define ALU_OP_SBC 0x03
#define ALU_OP_AND 0x04
#define ALU_OP_OR 0x05
#define ALU_OP_XOR 0x06
#define ALU_OP_CP 0x07
#define ALU_OP_INC 0x08
#define ALU_OP_DEC 0x09
#define ALU_OP_RLC 0x0A
#define ALU_OP_RL 0x0B
#define ALU_OP_RRC 0x0C
#define ALU_OP_RR 0x0D
#define ALU_OP_SLA 0x0E
#define ALU_OP_SRA 0x0F
#define ALU_OP_SRL 0x10
#define ALU_OP_BIT 0x11
#define ALU_OP_RES 0x12
#define ALU_OP_SET 0x13

// GP registers
#define REG_A 0x07
#define REG_B 0x00
#define REG_C 0x01
#define REG_D 0x02
#define REG_E 0x03
#define REG_H 0x04
#define REG_L 0x05

// GP Reg pairs
#define REG_BC 0x00
#define REG_DE 0x02
#define REG_HL 0x04
#define REG_AF 0x06


typedef struct {
  // Registers
  uint8_t regs[8];
  uint8_t shadow_regs[8];
  uint8_t flags, shadow_flags;
  uint16_t ix, iy; // Index Y and X
  uint16_t pc, sp; // Program counter, stack pointer
  uint16_t wz; // Temp register
  uint8_t i, r; // Interrupt, dram refresh
  uint8_t im; // Interrupt mode
  bool iff1, iff2;
  // Memory
  uint8_t memory[MEM_SIZE];
  bool halt;
} VirtZ80;

void execute(VirtZ80 *cpu);

void OutputHandler(uint8_t port, uint8_t value);
uint8_t InputHandler(uint8_t port);

uint8_t fByte(VirtZ80 *cpu);
int8_t fSByte(VirtZ80 *cpu);
uint16_t fWord(VirtZ80 *cpu);
void loadLow(uint16_t *reg, uint8_t value);
void loadHigh(uint16_t *reg, uint8_t value);
uint8_t getHigh(uint16_t reg);
uint8_t getLow(uint16_t reg);
void exchange(VirtZ80 *cpu, uint8_t regpair1, uint8_t regpair2);
void exchange_shadow(VirtZ80 *cpu, uint8_t regpair, uint8_t shadowregpair);
void push_new(VirtZ80 *cpu, uint8_t high, uint8_t low);
void pop_new(VirtZ80 *cpu, uint8_t* high, uint8_t* low);
void push(VirtZ80 *cpu, uint16_t value);
uint16_t pop(VirtZ80 *cpu);
uint16_t getRegpair(VirtZ80 *cpu, uint8_t regpair);
uint16_t getShadowRegpair(VirtZ80 *cpu, uint8_t regpair);
void loadRegpair(VirtZ80 *cpu, uint8_t regpair, uint16_t value);
void loadShadowRegpair(VirtZ80 *cpu, uint8_t regpair, uint16_t value);
void printState(VirtZ80 *cpu);
void stackTrace(VirtZ80 *cpu, int depth);

void MainInstruction(VirtZ80 *cpu);
void MiscInstruction(VirtZ80 *cpu);
void BitInstruction(VirtZ80 *cpu);
void IndexInstruction(VirtZ80 *cpu, uint16_t* index_reg);

/*DEBUG STUFF*/
void draw_registers(VirtZ80 *cpu);
void draw_memory(VirtZ80 *cpu);

char main_instructions[][0xFF] = {
  {"nop"},
  {"ld bc, 0x%04x"},
  {"ld (bc), a"},
  {"inc bc"},
  {"inc b"},
  {"dec b"},
};

#endif
