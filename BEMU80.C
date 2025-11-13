/* SPDX-License-Identifier: GPL-2.0-or-later
/Copyright (c) 2025 Benjamin Helle
*/ 
#include "BEMU80.H"
#include <cstdint>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/types.h>
#include <termios.h>

bool print_ins = false;

uint8_t memory[MEM_SIZE]; /* global memory */

/* register functions */
#define AF(cpu) ((cpu->regs[REG_A] << 8) | cpu->flags)
#define BC(cpu) ((cpu->regs[REG_B] << 8) | cpu->regs[REG_C])
#define DE(cpu) ((cpu->regs[REG_D] << 8) | cpu->regs[REG_E])
#define HL(cpu) ((cpu->regs[REG_H] << 8) | cpu->regs[REG_L])

#define EX(r1, r2) { uint16_t temp = r1; r1 = r2; r2 = temp; }

static inline void set_af(VirtZ80 *cpu, uint16_t value) { cpu->regs[REG_A] = (value >> 8) & 0xFF; cpu->flags = value & 0xFF; }
static inline void set_bc(VirtZ80 *cpu, uint16_t value) { cpu->regs[REG_B] = (value >> 8) & 0xFF; cpu->regs[REG_C] = value & 0xFF; }
static inline void set_de(VirtZ80 *cpu, uint16_t value) { cpu->regs[REG_D] = (value >> 8) & 0xFF; cpu->regs[REG_E] = value & 0xFF; }
static inline void set_hl(VirtZ80 *cpu, uint16_t value) { cpu->regs[REG_H] = (value >> 8) & 0xFF; cpu->regs[REG_L] = value & 0xFF; }

static inline void mwrite8(uint16_t address, uint8_t value) { 
  if (address >= MEM_SIZE || address < 0) return; /* TODO: error handling */
  memory[address] = value;
}

static inline uint8_t mread8(uint16_t address) {
  if (address >= MEM_SIZE || address < 0) return 0;
  return memory[address];
}

static inline void mwrite16(uint16_t address, uint16_t value) {
  if (address >= MEM_SIZE || address < 0) return;
  memory[address] = value & 0xFF;
  memory[address + 1] = (value >> 8) & 0xFF;
}

static inline uint16_t mread16( uint16_t address) {
  if (address >= MEM_SIZE || address < 0) return 0;
  return memory[address] | memory[address + 1] << 8;
}

static inline uint8_t fByte(VirtZ80 *cpu) {
  return mread8(cpu->pc++);
}

static inline uint16_t fWord(VirtZ80 *cpu) {
  uint16_t result = mread16(cpu->pc);
  cpu->pc += 2;
  return result;
}

static inline void push(VirtZ80 *cpu, uint16_t value) {
  cpu->sp -= 2;
  mwrite16(cpu->sp, value);
  //memory[--cpu->sp] = value >> 8;
  //memory[--cpu->sp] = value & 0xFF;  
}

static inline uint16_t pop(VirtZ80 *cpu) {
  uint16_t result = memory[cpu->sp++];
  result |= memory[cpu->sp++] << 8;
  return result;
}

int getch_in(void) {
  struct termios oldt, newt;
  int ch;

  tcgetattr(STDIN_FILENO, &oldt); // Save old settings
  newt = oldt;
  newt.c_lflag &= ~(ICANON | ECHO);         // Disable buffering and echo
  tcsetattr(STDIN_FILENO, TCSANOW, &newt);  // Apply new settings

  ch = getchar();  // Read one character

  tcsetattr(STDIN_FILENO, TCSANOW, &oldt);  // Restore old settings
  return ch;
}

void execute(VirtZ80 *cpu) {
  while (!cpu->halt) {
    //if (!stepping) usleep(500); // adjust delay
    if (print_ins) printf("Instruction: 0x%02x at 0x%04x | ", memory[cpu->pc], cpu->pc);
    if (print_ins) printState(cpu);
    MainInstruction(cpu);
    cpu->r += 1;
  }
}

void exchange(VirtZ80 *cpu, uint8_t regpair1, uint8_t regpair2, bool is_shadow) {
  uint16_t temp1 = cpu->regs[regpair1] << 8 | cpu->regs[regpair1 + 1];
  uint16_t temp2 = 0;

  if (is_shadow) temp2 = cpu->shadow_regs[regpair2] << 8 | cpu->shadow_regs[regpair2 + 1]; 
  else temp2 = cpu->regs[regpair2] << 8 | cpu->regs[regpair2 + 1];

  cpu->regs[regpair1] = temp2 >> 8;
  cpu->regs[regpair1 + 1] = temp2 & 0xFF;

  if (is_shadow) {
    cpu->shadow_regs[regpair1] = temp1 >> 8;
    cpu->shadow_regs[regpair1 + 1] = temp1 & 0xFF;
  } else {
    cpu->regs[regpair2] = temp1 >> 8;
    cpu->regs[regpair2 + 1] = temp1 & 0xFF;
  }
}

void exchange_af(VirtZ80 *cpu) {
  uint16_t temp1 = AF(cpu);
  uint16_t temp2 = cpu->shadow_regs[REG_A] << 8 | cpu->shadow_flags;

  set_af(cpu, temp2);

  cpu->shadow_regs[REG_A] = temp1 >> 8;
  cpu->shadow_flags = temp1 & 0xFF;
}

static inline uint8_t getFlag(VirtZ80 *cpu, uint8_t flag) {
  return (cpu->flags & flag) ? 1 : 0;
}

static inline void setFlag(VirtZ80 *cpu, uint8_t flag, uint8_t value) {
  if (value) cpu->flags |= flag;
  else cpu->flags &= ~flag;
}

void OutputHandler(uint8_t port, uint8_t value) {
  //printf("output port: %02x value: %02x\n", port, value);
  if (port == STD_PORT) { 
    printf("%c", value);
    fflush(stdout); // need to flush to work without '\n' newline. 
  } // STDOUT
}

uint8_t InputHandler(uint8_t port) {
  char input = 0;
  if (port == STD_PORT) input = (char)getch_in(); // STDIN
  //printf("ASCII CODE: %02x", input);
  return input;
}

/* Inline ALU operations */
static inline void update_flagsYX(VirtZ80 *cpu, uint8_t result) {
  if ((result & 0x20)) cpu->flags |= FLAG_Y; else cpu->flags &= ~FLAG_Y;
  if ((result & 0x08)) cpu->flags |= FLAG_X; else cpu->flags &= ~FLAG_X;
}

static inline uint8_t add8(VirtZ80 *cpu, uint8_t a, uint8_t b) {
  uint16_t res16 = (uint16_t)a + (uint16_t)b;
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;

  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if ((a ^ b ^ res8) & 0x10) new_f |= FLAG_H;
  if (((~(a ^ b) & (a ^ res8)) & 0x80) != 0) new_f |= FLAG_PV; //if ((~(a ^ b) & (a ^ res8)) & 0x80) new_f |= FLAG_PV;
  if (res16 & 0x100) new_f |= FLAG_C;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_H | FLAG_PV | FLAG_C)) | new_f;
  setFlag(cpu, FLAG_N, 0);

  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t adc8(VirtZ80 *cpu, uint8_t a, uint8_t b) {
  uint16_t res16 = (uint16_t)a + (uint16_t)b + (uint16_t)getFlag(cpu, FLAG_C);
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;

  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if ((a ^ b ^ res8) & 0x10) new_f |= FLAG_H;
  if (((~(a ^ b) & (a ^ res8)) & 0x80) != 0) new_f |= FLAG_PV;
  if (res16 & 0x100) new_f |= FLAG_C;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_H | FLAG_PV | FLAG_C)) | new_f;
  setFlag(cpu, FLAG_N, 0);

  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t sub8(VirtZ80 *cpu, uint8_t a, uint8_t b) {
  uint16_t res16 = (uint16_t)a - (uint16_t)b;
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;

  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if ((a ^ b ^ res8) & 0x10) new_f |= FLAG_H;
  if ((((a ^ b) & (a ^ res8)) & 0x80)) new_f |= FLAG_PV;
  if (res16 & 0x100) new_f |= FLAG_C;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_H | FLAG_PV | FLAG_C)) | new_f;
  setFlag(cpu, FLAG_N, 1);

  update_flagsYX(cpu, res8);

  return res8;
}

static inline void cp8(VirtZ80 *cpu, uint8_t a, uint8_t b) {
  uint16_t res16 = (uint16_t)a - b;
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;

  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if ((a ^ b ^ res8) & 0x10) new_f |= FLAG_H;
  if ((((a ^ b) & (a ^ res8)) & 0x80)) new_f |= FLAG_PV;
  if (res16 & 0x100) new_f |= FLAG_C;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_H | FLAG_PV | FLAG_C)) | new_f;
  setFlag(cpu, FLAG_N, 1);

  update_flagsYX(cpu, b);
}

static inline uint8_t sbc8(VirtZ80 *cpu, uint8_t a, uint8_t b) {
  uint16_t res16 = (uint16_t)a - (uint16_t)b - (uint16_t)getFlag(cpu, FLAG_C);
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;

  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if ((a ^ b ^ res8) & 0x10) new_f |= FLAG_H;
  if ((((a ^ b) & (a ^ res8)) & 0x80)) new_f |= FLAG_PV;
  if (res16 & 0x100) new_f |= FLAG_C;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_H | FLAG_PV | FLAG_C)) | new_f;
  setFlag(cpu, FLAG_N, 1);

  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t and8(VirtZ80 *cpu, uint8_t a, uint8_t b) {
  uint8_t res8 = a & b;
  uint8_t new_f = 0;

  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  new_f |= FLAG_H;
  if (__builtin_parity(res8) == 0) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_PV)) | new_f;
  setFlag(cpu, FLAG_N | FLAG_C, 0);

  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t or8(VirtZ80 *cpu, uint8_t a, uint8_t b) {
  uint8_t res8 = a | b;
  uint8_t new_f = 0;
  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if (__builtin_parity(res8) == 0) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_PV)) | new_f;
  setFlag(cpu, FLAG_N | FLAG_H | FLAG_C, 0);

  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t xor8(VirtZ80 *cpu, uint8_t a, uint8_t b) {
  uint8_t res8 = a ^ b;
  uint8_t new_f = 0;
  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if (__builtin_parity(res8) == 0) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_PV)) | new_f;
  setFlag(cpu, FLAG_N | FLAG_H | FLAG_C, 0);

  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t inc8(VirtZ80 *cpu, uint8_t a) {
  uint16_t res16 = (uint16_t)a + 1;
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;

  bool set_overflow = a == 0x7F ? true : false;

  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if ((a ^ res8) & 0x10) new_f |= FLAG_H;
  if (set_overflow) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_H | FLAG_PV)) | new_f;
  setFlag(cpu, FLAG_N, 0);

  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t dec8(VirtZ80 *cpu, uint8_t a) {
  uint16_t res16 = (uint16_t)a - 1;
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;

  bool set_overflow = a == 0x80 ? true : false;

  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if ((a ^ res8) & 0x10) new_f |= FLAG_H;
  if (set_overflow) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_H | FLAG_PV)) | new_f;
  setFlag(cpu, FLAG_N, 1);

  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint16_t add16(VirtZ80 *cpu, uint16_t a, uint16_t b) {
  uint32_t res32 = (uint32_t)a + (uint32_t)b;
  uint16_t res16 = (uint16_t)res32;
  uint8_t new_f = 0;

  if ((a ^ b ^ res16) & 0x1000) new_f |= FLAG_H;
  if (res32 & 0x10000) new_f |= FLAG_C;

  cpu->flags = (cpu->flags & ~(FLAG_H | FLAG_C)) | new_f;
  setFlag(cpu, FLAG_N, 0);

  update_flagsYX(cpu, (res16 >> 8)); /* Higher byte*/

  return res16;
}

static inline uint16_t adc16(VirtZ80 *cpu, uint16_t a, uint16_t b) {
  uint32_t res32 = (uint32_t)a + (uint32_t)b + (uint32_t)getFlag(cpu, FLAG_C);
  uint16_t res16 = (uint16_t)res32;
  uint8_t new_f = 0;

  if ((a ^ b ^ res16) & 0x1000) new_f |= FLAG_H;
  if (res32 & 0x10000) new_f |= FLAG_C;
  if (res16 & 0x8000) new_f |= FLAG_S;

  if (res16 == 0) new_f |= FLAG_Z;
    if ((((a ^ b) & (a ^ res16)) & 0x8000)) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_H | FLAG_PV | FLAG_C)) | new_f;
  setFlag(cpu, FLAG_N, 0);

  update_flagsYX(cpu, (res16 >> 8)); /* Higher byte*/

  return res16;
}

static inline uint16_t sbc16(VirtZ80 *cpu, uint16_t a, uint16_t b) {
  uint32_t res32 = (uint32_t)a - (uint32_t)b - (uint32_t)getFlag(cpu, FLAG_C);
  uint16_t res16 = (uint16_t)res32;
  uint8_t new_f = 0;

  if ((a ^ b ^ res16) & 0x1000) new_f |= FLAG_H;
  if (res32 & 0x10000) new_f |= FLAG_C;
  if (res16 & 0x8000) new_f |= FLAG_S;

  if (res16 == 0) new_f |= FLAG_Z;
    if ((((a ^ b) & (a ^ res16)) & 0x8000)) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_H | FLAG_PV | FLAG_C)) | new_f;
  setFlag(cpu, FLAG_N, 1);

  update_flagsYX(cpu, (res16 >> 8)); /* Higher byte*/

  return res16;
}

static inline uint16_t inc16(uint16_t a) {
  return a + 1;
}

static inline uint16_t dec16(uint16_t a) {
  return a - 1;
}

/* Inline rotate */

static inline uint8_t rlc8(VirtZ80 *cpu, uint8_t a) {
  uint16_t res16 = (a << 1) | (a >> 7);
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;
  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if (__builtin_parity(res8) == 0) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_PV)) | new_f;
  setFlag(cpu, FLAG_N | FLAG_H, 0);

  setFlag(cpu, FLAG_C, a & 0x80);

  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t rl8(VirtZ80 *cpu, uint8_t a) {
  uint16_t res16 = (a << 1) | getFlag(cpu, FLAG_C);
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;
  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if (__builtin_parity(res8) == 0) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_PV)) | new_f;
  setFlag(cpu, FLAG_N | FLAG_H, 0);

  setFlag(cpu, FLAG_C, a & 0x80);
  
  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t rrc8(VirtZ80 *cpu, uint8_t a) {
  uint16_t res16 = (a >> 1) | (a << 7);
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;
  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if (__builtin_parity(res8) == 0) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_PV)) | new_f;
  setFlag(cpu, FLAG_N | FLAG_H, 0);

  setFlag(cpu, FLAG_C, a & 0x01);
  
  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t rr8(VirtZ80 *cpu, uint8_t a) {
  uint16_t res16 = (a >> 1) | (getFlag(cpu, FLAG_C) << 7);
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;
  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if (__builtin_parity(res8) == 0) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_PV)) | new_f;
  setFlag(cpu, FLAG_N | FLAG_H, 0);

  setFlag(cpu, FLAG_C, a & 0x01);
  
  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t sla8(VirtZ80 *cpu, uint8_t a) {
  uint16_t res16 = (a << 1) | 0;
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;
  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if (__builtin_parity(res8) == 0) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_PV)) | new_f;
  setFlag(cpu, FLAG_N | FLAG_H, 0);

  setFlag(cpu, FLAG_C, a & 0x80);
  
  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t sll8(VirtZ80 *cpu, uint8_t a) {
  uint16_t res16 = (a << 1) | 1;
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;
  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if (__builtin_parity(res8) == 0) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_PV)) | new_f;
  setFlag(cpu, FLAG_N | FLAG_H, 0);

  setFlag(cpu, FLAG_C, a & 0x80);
  
  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t sra8(VirtZ80 *cpu, uint8_t a) {
  uint16_t res16 = (a >> 1) | (a & 0x80);
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;
  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if (__builtin_parity(res8) == 0) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_PV)) | new_f;
  setFlag(cpu, FLAG_N | FLAG_H, 0);

  setFlag(cpu, FLAG_C, a & 0x01);
  
  update_flagsYX(cpu, res8);

  return res8;
}

static inline uint8_t srl8(VirtZ80 *cpu, uint8_t a) {
  uint16_t res16 = (a >> 1);
  uint8_t res8 = (uint8_t)res16;
  uint8_t new_f = 0;
  if (res8 & 0x80) new_f |= FLAG_S;
  if (res8 == 0) new_f |= FLAG_Z;
  if (__builtin_parity(res8) == 0) new_f |= FLAG_PV;

  cpu->flags = (cpu->flags & ~(FLAG_S | FLAG_Z | FLAG_PV)) | new_f;
  setFlag(cpu, FLAG_N | FLAG_H, 0);

  setFlag(cpu, FLAG_C, a & 0x01);
  
  update_flagsYX(cpu, res8);

  return res8;
}

static inline void bit8(VirtZ80 *cpu, uint8_t a, uint8_t bit) {
  bool result = a & (1 << bit);
  setFlag(cpu, FLAG_PV | FLAG_Z, !result);
  setFlag(cpu, FLAG_H, 1);
  setFlag(cpu, FLAG_N, 0);
  if (result) {
    setFlag(cpu, FLAG_S, a & 0x80);
    setFlag(cpu, FLAG_Y, a & 0x20);
    setFlag(cpu, FLAG_X, a & 0x08);
  } else cpu->flags &= ~(FLAG_S | FLAG_Y | FLAG_X);
}

static inline uint8_t res8(VirtZ80 *cpu, uint8_t a, uint8_t bit) {
  return (a & ~(1 << bit));
}

static inline uint8_t set8(VirtZ80 *cpu, uint8_t a, uint8_t bit) {
  return (a | (1 << bit));
}

void MainInstruction(VirtZ80 *cpu) {
  uint8_t opcode = fByte(cpu);
  int8_t reladdr = 0;
  switch (opcode) {
    case 0x00: // NOP
      break;
    case 0x01: // LD BC, nn
      set_bc(cpu, fWord(cpu));
      break;
    case 0x02: // LD (BC), A
      mwrite8(BC(cpu), cpu->regs[REG_A]);
      break;
    case 0x03: // INC BC
      set_bc(cpu, inc16(BC(cpu)));
      break;
    case 0x04: // INC B
      cpu->regs[REG_B] = inc8(cpu, cpu->regs[REG_B]);
      break;
    case 0x05: // DEC B
      cpu->regs[REG_B] = dec8(cpu, cpu->regs[REG_B]);
      break;
    case 0x06: // LD B, n
      cpu->regs[REG_B] = fByte(cpu);
      break;
    case 0x07: // RLCA
      { /* Dirty way to do this */
        uint16_t result = (cpu->regs[REG_A] << 1) | (cpu->regs[REG_A] >> 7);
        //if (result & 0x100) setFlag(cpu, FLAG_C, 1);
        //else setFlag(cpu, FLAG_C, 0);
        setFlag(cpu, FLAG_C, (result & 0x100) >> 8);
        setFlag(cpu, FLAG_N | FLAG_H, 0);

        cpu->regs[REG_A] = result & 0xFF;

        if (cpu->regs[REG_A] & 0x08) setFlag(cpu, FLAG_Y, 1); else setFlag(cpu, FLAG_Y, 0);
        if (cpu->regs[REG_A] & 0x20) setFlag(cpu, FLAG_X, 1); else setFlag(cpu, FLAG_X, 0);
      }
      break;
    case 0x08: // EX AF, AF'
      exchange_af(cpu);
      break;
    case 0x09: // ADD HL, BC
      set_hl(cpu, add16(cpu, HL(cpu), BC(cpu)));
      break;
    case 0x0A: // LD A, (BC)
      cpu->regs[REG_A] = mread8(BC(cpu));
      break;
    case 0x0B: // DEC BC
      set_bc(cpu, dec16(BC(cpu)));
      break;
    case 0x0C: // INC C
      cpu->regs[REG_C] = inc8(cpu, cpu->regs[REG_C]);
      break;
    case 0x0D: // DEC C
      cpu->regs[REG_C] = dec8(cpu, cpu->regs[REG_C]);
      break;
    case 0x0E: // LD C, n
      cpu->regs[REG_C] = fByte(cpu);
      break;
    case 0x0F: // RRCA
      {
        uint16_t result = (cpu->regs[REG_A] >> 1) | (cpu->regs[REG_A] << 7);
        setFlag(cpu, FLAG_C, cpu->regs[REG_A] & 0x01);
        cpu->regs[REG_A] = result & 0xFF;

        if (cpu->regs[REG_A] & 0x08) setFlag(cpu, FLAG_Y, 1); else setFlag(cpu, FLAG_Y, 0);
        if (cpu->regs[REG_A] & 0x20) setFlag(cpu, FLAG_X, 1); else setFlag(cpu, FLAG_X, 0);
      }
      break;
    case 0x10: // DJNZ d
      cpu->regs[REG_B] = dec8(cpu, cpu->regs[REG_B]);
      reladdr = (int8_t)fByte(cpu);
      if (cpu->regs[REG_B] != 0) {
        cpu->pc += (int8_t)reladdr; /* Treat as signed */
      }
      break;
    case 0x11: // LD DE, nn
      set_de(cpu, fWord(cpu));
      break;
    case 0x12: // LD (DE), A
      mwrite8(DE(cpu), cpu->regs[REG_A]);
      break;
    case 0x13: // INC DE
      set_de(cpu, inc16(DE(cpu)));
      break;
    case 0x14: // INC D
      cpu->regs[REG_D] = inc8(cpu, cpu->regs[REG_D]);
      break;
    case 0x15: // DEC D
      cpu->regs[REG_D] = dec8(cpu, cpu->regs[REG_D]);
      break;
    case 0x16: // LD D, n
      cpu->regs[REG_D] = fByte(cpu);
      break;
    case 0x17: // RLA
      {
        uint16_t result = (cpu->regs[REG_A] << 1) | (cpu->flags & FLAG_C);
        setFlag(cpu, FLAG_C, (result & 0x100) >> 8);
        setFlag(cpu, FLAG_N | FLAG_H, 0);

        cpu->regs[REG_A] = result & 0xFF;

        if (cpu->regs[REG_A] & 0x08) setFlag(cpu, FLAG_Y, 1); else setFlag(cpu, FLAG_Y, 0);
        if (cpu->regs[REG_A] & 0x20) setFlag(cpu, FLAG_X, 1); else setFlag(cpu, FLAG_X, 0);
      }
      break;
    case 0x18: // JR d
      cpu->pc += (int8_t)fByte(cpu);
      break;
    case 0x19: // ADD HL, DE
      set_hl(cpu, add16(cpu, HL(cpu), DE(cpu)));
      break;
    case 0x1A: // LD A, (DE)
      cpu->regs[REG_A] = mread8(DE(cpu));
      break;
    case 0x1B: // DEC DE
      set_de(cpu, dec16(DE(cpu)));
      break;
    case 0x1C: // INC E
      cpu->regs[REG_E] = inc8(cpu, cpu->regs[REG_E]);
      break;
    case 0x1D: // DEC E
      cpu->regs[REG_E] = dec8(cpu, cpu->regs[REG_E]);
      break;
    case 0x1E: // LD E, n
      cpu->regs[REG_E] = fByte(cpu);
      break;
    case 0x1F: // RRA
      {
        uint16_t result = (cpu->regs[REG_A] >> 1) | ((cpu->flags & FLAG_C) << 7);
        setFlag(cpu, FLAG_C, cpu->regs[REG_A] & 0x01);
        cpu->regs[REG_A] = result & 0xFF;

        if (cpu->regs[REG_A] & 0x08) setFlag(cpu, FLAG_Y, 1); else setFlag(cpu, FLAG_Y, 0);
        if (cpu->regs[REG_A] & 0x20) setFlag(cpu, FLAG_X, 1); else setFlag(cpu, FLAG_X, 0);
      }
      break;
    case 0x20: // JR NZ, d
      reladdr = (int8_t)fByte(cpu);
      if (getFlag(cpu, FLAG_Z) == 0) {
        cpu->pc += (int8_t)reladdr;
      }
      break;
    case 0x21: // LD HL, nn
      set_hl(cpu, fWord(cpu));
      break;
    case 0x22: // LD (nn), HL
      mwrite16(fWord(cpu), HL(cpu));
      //memory[cpu->wz] = cpu->regs[REG_L];
      //memory[cpu->wz + 1] = cpu->regs[REG_H];
      break;
    case 0x23: // INC HL
      set_hl(cpu, inc16(HL(cpu)));
      break;
    case 0x24: // INC H
      cpu->regs[REG_H] = inc8(cpu, cpu->regs[REG_H]);
      break;
    case 0x25: // DEC H
      cpu->regs[REG_H] = dec8(cpu, cpu->regs[REG_H]);
      break;
    case 0x26: // LD H, n
      cpu->regs[REG_H] = fByte(cpu);
      break;
    case 0x27: // DAA
      // TODO
      break;
    case 0x28: // JR Z, d
      reladdr = (int8_t)fByte(cpu);
      if (getFlag(cpu, FLAG_Z) == 1) {
        cpu->pc += (int8_t)reladdr;
      }
      break;
    case 0x29: // ADD HL, HL
      set_hl(cpu, add16(cpu, HL(cpu), HL(cpu)));
      break;
    case 0x2A: // LD HL, (nn)
      set_hl(cpu, mread16(fWord(cpu)));
      /*cpu->wz = fWord(cpu);
      cpu->regs[REG_L] = memory[cpu->wz];
      cpu->regs[REG_H] = memory[cpu->wz + 1];*/
      break;
    case 0x2B: // DEC HL
      set_hl(cpu, dec16(HL(cpu)));
      break;
    case 0x2C: // INC L
      cpu->regs[REG_L] = inc8(cpu, cpu->regs[REG_L]);
      break;
    case 0x2D: // DEC L
      cpu->regs[REG_L] = dec8(cpu, cpu->regs[REG_L]);
      break;
    case 0x2E: // LD L, n
      cpu->regs[REG_L] = fByte(cpu);
      break;
    case 0x2F: // CPL
      cpu->regs[REG_A] = ~cpu->regs[REG_A];
      setFlag(cpu, FLAG_N | FLAG_H, 1);
      break;
    case 0x30: // JR NC, d
      reladdr = (int8_t)fByte(cpu);
      if (getFlag(cpu, FLAG_C) == 0) {
        cpu->pc += (int8_t)reladdr;
        break;
      }
      break;
    case 0x31: // LD SP, nn
      cpu->sp = fWord(cpu);
      break;
    case 0x32: // LD (nn), A
      mwrite8(fWord(cpu), cpu->regs[REG_A]);
      /*cpu->wz = fWord(cpu);
      memory[cpu->wz] = cpu->regs[REG_A];*/
      break;
    case 0x33: // INC SP
      cpu->sp += 1;
      break;
    case 0x34: // INC (HL)
      mwrite8(HL(cpu), inc8(cpu, mread8(HL(cpu))));
      break;
    case 0x35: // DEC (HL)
      mwrite8(HL(cpu), dec8(cpu, mread8(HL(cpu))));
      break;
    case 0x36: // LD (HL), n
      mwrite8(HL(cpu), fByte(cpu));
      break;
    case 0x37: // SCF
      setFlag(cpu, FLAG_C, 1);
      break;
    case 0x38: // JR C, d
      reladdr = (int8_t)fByte(cpu);
      if (getFlag(cpu, FLAG_C) == 1) {
        cpu->pc += (int8_t)reladdr;
      }
      break;
    case 0x39: // ADD HL, SP
      set_hl(cpu, add16(cpu, HL(cpu), cpu->sp));
      break;
    case 0x3A: // LD A, (nn)
      cpu->regs[REG_A] = mread8(fWord(cpu));
      break;
    case 0x3B: // DEC SP
      cpu->sp -= 1;
      break;
    case 0x3C: // INC A
      cpu->regs[REG_A] = inc8(cpu, cpu->regs[REG_A]);
      break;
    case 0x3D: // DEC A
      cpu->regs[REG_A] = dec8(cpu, cpu->regs[REG_A]);
      break;
    case 0x3E: // LD A, n
      cpu->regs[REG_A] = fByte(cpu);
      break;
    case 0x3F: // CCF
      setFlag(cpu, FLAG_C, getFlag(cpu, FLAG_C) ^ 1);
      break;
    case 0x40: /*LD B, reg */
    case 0x41:
    case 0x42: 
    case 0x43:
    case 0x44:
    case 0x45:
    case 0x47:
      cpu->regs[REG_B] = cpu->regs[opcode & 0x07];
      break;
    case 0x46: // LD B, (HL)
      cpu->regs[REG_B] = mread8(HL(cpu));
      break;
    case 0x48: // LD C, reg
    case 0x49:
    case 0x4A:
    case 0x4B:
    case 0x4C:
    case 0x4D:
    case 0x4F:
      cpu->regs[REG_C] = cpu->regs[(opcode-8) & 0x07];
      break;
    case 0x4E: // LD C, (HL)
      cpu->regs[REG_C] = mread8(HL(cpu));
      break;

    case 0x50: // LD D, reg
    case 0x51:
    case 0x52:
    case 0x53:
    case 0x54:
    case 0x55:
    case 0x57:
      cpu->regs[REG_D] = cpu->regs[opcode & 0x07];
      break;
    case 0x56: // LD D, (HL)
      cpu->regs[REG_D] = mread8(HL(cpu));
      break;

    case 0x58: // LD E, reg
    case 0x59:
    case 0x5A:
    case 0x5B:
    case 0x5C:
    case 0x5D:
    case 0x5F:
      cpu->regs[REG_E] = cpu->regs[(opcode-8) & 0x07];
      break;
    case 0x5E: // LD E, (HL)
      cpu->regs[REG_E] = mread8(HL(cpu));
      break;

    case 0x60: // LD H, reg
    case 0x61:
    case 0x62:
    case 0x63:
    case 0x64:
    case 0x65:
    case 0x67:
      cpu->regs[REG_H] = cpu->regs[opcode & 0x07];
      break;
    case 0x66: // LD H, (HL)
      cpu->regs[REG_H] = mread8(HL(cpu));
      break;

    case 0x68: // LD L, reg
    case 0x69:
    case 0x6A:
    case 0x6B:
    case 0x6C:
    case 0x6D:
    case 0x6F:
      cpu->regs[REG_L] = cpu->regs[(opcode-8) & 0x07];
      break;
    case 0x6E: // LD L, (HL)
      cpu->regs[REG_L] = mread8(HL(cpu));
      break;

    case 0x70: // LD (HL), reg
    case 0x71:
    case 0x72:
    case 0x73:
    case 0x74:
    case 0x75:
    case 0x77:
      mwrite8(HL(cpu), cpu->regs[opcode & 0x07]);
      break;

    case 0x76: // HALT
      cpu->halt = true;
      cpu->pc--; /* Don't increment PC*/
      break;

    case 0x78: // LD A, reg
    case 0x79:
    case 0x7A:
    case 0x7B:
    case 0x7C:
    case 0x7D:
    case 0x7F:
      cpu->regs[REG_A] = cpu->regs[(opcode-8) & 0x07];
      break;
    case 0x7E: // LD A, (HL)
      cpu->regs[REG_A] = mread8(HL(cpu));
      break;

    case 0x80: // ADD A, reg
    case 0x81:
    case 0x82:
    case 0x83:
    case 0x84:
    case 0x85:
    case 0x87:
      cpu->regs[REG_A] = add8(cpu, cpu->regs[REG_A], cpu->regs[(opcode & 0x07)]);
      break;
    case 0x86: // ADD A, (HL)
      cpu->regs[REG_A] = add8(cpu, cpu->regs[REG_A], mread8(HL(cpu)));
      break;

    case 0x88: // ADC A, reg
    case 0x89:
    case 0x8A:
    case 0x8B:
    case 0x8C:
    case 0x8D:
    case 0x8F:
      cpu->regs[REG_A] = adc8(cpu, cpu->regs[REG_A], cpu->regs[((opcode-8) & 0x07)]);
      break;
    case 0x8E: // ADC A, (HL)
      cpu->regs[REG_A] = adc8(cpu, cpu->regs[REG_A], mread8(HL(cpu)));
      break;

    case 0x90: // SUB A, reg
    case 0x91:
    case 0x92:
    case 0x93:
    case 0x94:
    case 0x95:
    case 0x97:
      cpu->regs[REG_A] = sub8(cpu, cpu->regs[REG_A], cpu->regs[(opcode & 0x07)]);
      break;
    case 0x96: // SUB A, (HL)
      cpu->regs[REG_A] = sub8(cpu, cpu->regs[REG_A], mread8(HL(cpu)));
      break;

    case 0x98: // SBC A, reg
    case 0x99:
    case 0x9A:
    case 0x9B:
    case 0x9C:
    case 0x9D:
    case 0x9F:
      cpu->regs[REG_A] = sbc8(cpu, cpu->regs[REG_A], cpu->regs[((opcode-8) & 0x07)]);
      break;
    case 0x9E: // SBC A, (HL)
      cpu->regs[REG_A] = sbc8(cpu, cpu->regs[REG_A], mread8(HL(cpu)));
      break;

    case 0xA0: // AND A, reg
    case 0xA1:
    case 0xA2:
    case 0xA3:
    case 0xA4:
    case 0xA5:
    case 0xA7:
      cpu->regs[REG_A] = and8(cpu, cpu->regs[REG_A], cpu->regs[(opcode & 0x07)]);
      break;
    case 0xA6: // AND A, (HL)
      cpu->regs[REG_A] = and8(cpu, cpu->regs[REG_A], mread8(HL(cpu)));
      break;

    case 0xA8: // XOR A, reg
    case 0xA9:
    case 0xAA:
    case 0xAB:
    case 0xAC:
    case 0xAD:
    case 0xAF:
      cpu->regs[REG_A] = xor8(cpu, cpu->regs[REG_A], cpu->regs[((opcode-8) & 0x07)]);
      break;
    case 0xAE: // XOR A, (HL)
      cpu->regs[REG_A] = xor8(cpu, cpu->regs[REG_A], mread8(HL(cpu)));
      break;

    case 0xB0: // OR A, reg
    case 0xB1:
    case 0xB2:
    case 0xB3:
    case 0xB4:
    case 0xB5:
    case 0xB7:
      cpu->regs[REG_A] = or8(cpu, cpu->regs[REG_A], cpu->regs[(opcode & 0x07)]);
      break;
    case 0xB6: // OR A, (HL)
      cpu->regs[REG_A] = or8(cpu, cpu->regs[REG_A], mread8(HL(cpu)));
      break;

    case 0xB8: // CP A, reg
    case 0xB9:
    case 0xBA:
    case 0xBB:
    case 0xBC:
    case 0xBD:
    case 0xBF:
      cp8(cpu, cpu->regs[REG_A], cpu->regs[((opcode-8) & 0x07)]); /* Same as sub, but doesn't modify A */
      break;
    case 0xBE: // CP A, (HL)
      cp8(cpu, cpu->regs[REG_A], mread8(HL(cpu)));
      break;
    
    case 0xC0: // RET NZ
      if (getFlag(cpu, FLAG_Z) == 0) {
        cpu->pc = pop(cpu);
      }
      break;
    case 0xC1: // POP BC
      set_bc(cpu, pop(cpu));
      break;
    case 0xC2: // JP NZ, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_Z) == 0) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xC3: // JP nn
      cpu->wz = fWord(cpu);
      cpu->pc = cpu->wz;
      break;
    case 0xC4: // CALL NZ, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_Z) == 0) {
        push(cpu, cpu->pc);
        cpu->pc = cpu->wz;
      }
      break;
    case 0xC5: // PUSH BC
      push(cpu, BC(cpu));
      break;
    case 0xC6: // ADD A, n
      cpu->regs[REG_A] = add8(cpu, cpu->regs[REG_A], fByte(cpu));
      break;
    case 0xC7: // RST 0h
      push(cpu, cpu->pc);
      cpu->pc = 0x00;
      break;
    case 0xC8: // RET Z
      if (getFlag(cpu, FLAG_Z)) {
        cpu->pc = pop(cpu);
        break;
      }
      break;
    case 0xC9: // RET
      cpu->pc = pop(cpu);
      break;
    case 0xCA: // JP Z, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_Z)) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xCB: // Bit instruction
      BitInstruction(cpu);
      break;
    case 0xCC: // CALL Z, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_Z)) {
        push(cpu, cpu->pc);
        cpu->pc = cpu->wz;
      }
      break;
    case 0xCD: // CALL nn
      cpu->wz = fWord(cpu);
      push(cpu, cpu->pc);
      cpu->pc = cpu->wz;
      break;
    case 0xCE: // ADC A, n
      cpu->regs[REG_A] = adc8(cpu, cpu->regs[REG_A], fByte(cpu));
      break;
    case 0xCF: // RST 8h
      push(cpu, cpu->pc);
      cpu->pc = 0x08;
      break;
    case 0xD0: // RET NC
      if (getFlag(cpu, FLAG_C) == 0) {
        cpu->pc = pop(cpu);
      }
      break;
    case 0xD1: // POP DE
      set_de(cpu, pop(cpu));
      break;
    case 0xD2: // JP NC, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_C) == 0) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xD3: // OUT (n), A
      OutputHandler(fByte(cpu), cpu->regs[REG_A]);
      break;
    case 0xD4: // CALL NC, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_C) == 0) {
        push(cpu, cpu->pc);
        cpu->pc = cpu->wz;
      }
      break;
    case 0xD5: // PUSH DE
      push(cpu, DE(cpu));
      break;
    case 0xD6: // SUB A, n
      cpu->regs[REG_A] = sub8(cpu, cpu->regs[REG_A], fByte(cpu));
      break;
    case 0xD7: // RST 10h
      push(cpu, cpu->pc);
      cpu->pc = 0x10;
      break;
    case 0xD8: // RET C
      if (getFlag(cpu, FLAG_C)) {
        cpu->pc = pop(cpu);
      }
      break;
    case 0xD9: // EXX
      exchange(cpu, REG_BC, REG_BC, true);
      exchange(cpu, REG_DE, REG_DE, true);
      exchange(cpu, REG_HL, REG_HL, true);
      break;
    case 0xDA: // JP C, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_C)) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xDB: // IN A, (n)
      cpu->regs[REG_A] = InputHandler(fByte(cpu));
      break;
    case 0xDC: // CALL C, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_C)) {
        push(cpu, cpu->pc);
        cpu->pc = cpu->wz;
      }
      break;
    case 0xDD: // IX Prefix
      IndexInstruction(cpu, &cpu->ix);
      break;
    case 0xDE: // SBC A, n
      cpu->regs[REG_A] = sbc8(cpu, cpu->regs[REG_A], fByte(cpu));
      break;
    case 0xDF: // RST 18h
      push(cpu, cpu->pc);
      cpu->pc = 0x18;
      break;
    case 0xE0: // RET PO
      if (getFlag(cpu, FLAG_PV) == 0) {
        cpu->pc = pop(cpu);
      }
      break;
    case 0xE1: // POP HL
      set_hl(cpu, pop(cpu));
      break;
    case 0xE2: // JP PO, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_PV) == 0) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xE3: // EX (SP),HL
      cpu->wz = HL(cpu);
      set_hl(cpu, mread16(cpu->sp));
      mwrite16(cpu->sp, cpu->wz);
      break;
    case 0xE4: // CALL PO, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_PV) == 0) {
        push(cpu, cpu->pc);
        cpu->pc = cpu->wz;
      }
      break;
    case 0xE5: // PUSH HL
      push(cpu, HL(cpu)); 
      break;
    case 0xE6: // AND A, n
      cpu->regs[REG_A] = and8(cpu, cpu->regs[REG_A], fByte(cpu));
      break;
    case 0xE7: // RST 20h
      push(cpu, cpu->pc);
      cpu->pc = 0x20;
      break;
    case 0xE8: // RET PE
      if (getFlag(cpu, FLAG_PV)) {
        cpu->pc = pop(cpu);
      }
      break;
    case 0xE9: // JP (HL)
      cpu->pc = HL(cpu);
      break;
    case 0xEA: // JP PE, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_PV)) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xEB: // EX DE,HL
      exchange(cpu, REG_DE, REG_HL, false);
      break;
    case 0xEC: // CALL PE, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_PV)) {
        push(cpu, cpu->pc);
        cpu->pc = cpu->wz;
      }
      break;
    case 0xED: // Misc. Instructions
      MiscInstruction(cpu);
      break;
    case 0xEE: // XOR A, n
      cpu->regs[REG_A] = xor8(cpu, cpu->regs[REG_A], fByte(cpu));
      break;
    case 0xEF: // RST 28h
      push(cpu, cpu->pc);
      cpu->pc = 0x28;
      break;
    case 0xF0: // RET P
      if (getFlag(cpu, FLAG_S) == 0) {
        cpu->pc = pop(cpu);
      }
      break;
    case 0xF1: // POP AF
      set_af(cpu, pop(cpu));
      break;
    case 0xF2: // JP P, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_S) == 0) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xF3: // DI
      cpu->iff2 = 0;
      break;
    case 0xF4: // CALL P, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_S) == 0) {
        push(cpu, cpu->pc);
        cpu->pc = cpu->wz;
      }
      break;
    case 0xF5: // PUSH AF
      push(cpu, AF(cpu));
      break;
    case 0xF6: // OR A, n
      cpu->regs[REG_A] = or8(cpu, cpu->regs[REG_A], fByte(cpu));
      break;
    case 0xF7: // RST 30h
      push(cpu, cpu->pc);
      cpu->pc = 0x30;
      break;
    case 0xF8: // RET M
      if (getFlag(cpu, FLAG_S)) {
        cpu->pc = pop(cpu);
      }
      break;
    case 0xF9: // LD SP, HL
      cpu->sp = HL(cpu);
      break;
    case 0xFA: // JP M, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_S)) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xFB: // EI
      cpu->iff1 = 1;
      break;
    case 0xFC: // CALL M, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_S)) {
        push(cpu, cpu->pc);
        cpu->pc = cpu->wz;
      }
      break;
    case 0xFD: // IY Prefix
      IndexInstruction(cpu, &cpu->iy);
      break;
    case 0xFE: // CP A, n
      cp8(cpu, cpu->regs[REG_A], fByte(cpu));
      break;
    case 0xFF: // RST 38h
      push(cpu, cpu->pc);
      cpu->pc = 0x38;
      break;
    default:
      break;
  }
}

void MiscInstruction(VirtZ80 *cpu) {
  uint8_t opcode = fByte(cpu);
  uint8_t temp1 = 0;
  if (print_ins) printf("0xED Instruction: 0x%02x\n", opcode);
  switch (opcode) {
    case 0x40: // IN B, (C)
      cpu->regs[REG_B] = InputHandler(cpu->regs[REG_C]);
      break;
    case 0x41: // OUT (C), B
      OutputHandler(cpu->regs[REG_C], cpu->regs[REG_B]);
      break;
    case 0x42: // SBC HL, BC
      set_hl(cpu, sbc16(cpu, HL(cpu), BC(cpu)));
      break;
    case 0x43: // LD (nn), BC
      mwrite16(fWord(cpu), BC(cpu));
      break;
    case 0x44: // NEG
      // Check if working!!
      cpu->regs[REG_A] = sub8(cpu, 0, cpu->regs[REG_A]);
      break;
    case 0x45: // RETN
      cpu->pc = pop(cpu);
      cpu->iff1 = cpu->iff2;
      break;
    case 0x46: // IM 0
      cpu->im = 0;
      break;
    case 0x47: // LD I, A
      cpu->i = cpu->regs[REG_A];
      break;
    case 0x48: // IN C, (C)
      cpu->regs[REG_C] = InputHandler(cpu->regs[REG_C]);
      break;
    case 0x49: // OUT (C), C
      OutputHandler(cpu->regs[REG_C], cpu->regs[REG_C]);
      break;
    case 0x4A: // ADC HL, BC
      set_hl(cpu, adc16(cpu, HL(cpu), BC(cpu)));
      break;
    case 0x4B: // LD BC, (nn)
      set_bc(cpu, mread16(fWord(cpu)));
      /*cpu->wz = fWord(cpu);
      cpu->regs[REG_C] = memory[cpu->wz];
      cpu->regs[REG_B] = memory[cpu->wz + 1];*/
      break;
    case 0x4D: // RETI
      cpu->pc = pop(cpu);
      cpu->iff1 = 1;
      cpu->iff2 = 1;
      break;
    case 0x4F: // LD R, A
      cpu->r = cpu->regs[REG_A];
      break;
    case 0x50: // IN D, (C)
      cpu->regs[REG_D] = InputHandler(cpu->regs[REG_C]);
      break;
    case 0x51: // OUT (C), D
      OutputHandler(cpu->regs[REG_C], cpu->regs[REG_D]);
      break;
    case 0x52: // SBC HL, DE
      set_hl(cpu, sbc16(cpu, HL(cpu), DE(cpu)));
      break;
    case 0x53: // LD (nn), DE
      mwrite16(fWord(cpu), DE(cpu));
      break;
    case 0x56: // IM 1
      cpu->im = 1;
      break;
    case 0x57: // LD A, I
      cpu->regs[REG_A] = cpu->i;
      setFlag(cpu, FLAG_S, cpu->i & 0x80);
      setFlag(cpu, FLAG_Z, cpu->i == 0);
      setFlag(cpu, FLAG_N | FLAG_H, 0);
      setFlag(cpu, FLAG_PV, cpu->iff2);
      break;
    case 0x58: // IN E, (C)
      cpu->regs[REG_E] = InputHandler(cpu->regs[REG_C]);
      break;
    case 0x59: // OUT (C), E
      OutputHandler(cpu->regs[REG_C], cpu->regs[REG_E]);
      break;
    case 0x5A: // ADC HL, DE
      set_hl(cpu, adc16(cpu, HL(cpu), DE(cpu)));
      break;
    case 0x5B: // LD DE, (nn)
      cpu->wz = fWord(cpu);
      cpu->regs[REG_E] = memory[cpu->wz];
      cpu->regs[REG_D] = memory[cpu->wz + 1];
      break;
    case 0x5E: // IM 2
      cpu->im = 2;
      break;
    case 0x5F: // LD A, R
      cpu->regs[REG_A] = cpu->r;
      setFlag(cpu, FLAG_S, cpu->r & 0x80);
      setFlag(cpu, FLAG_Z, cpu->r == 0);
      setFlag(cpu, FLAG_N | FLAG_H, 0);
      setFlag(cpu, FLAG_PV, cpu->iff2);
      break;
    case 0x60: // IN H, (C)
      cpu->regs[REG_H] = InputHandler(cpu->regs[REG_C]);
      break;
    case 0x61: // OUT (C), H
      OutputHandler(cpu->regs[REG_C], cpu->regs[REG_H]);
      break;
    case 0x62: // SBC HL, HL
      set_hl(cpu, sbc16(cpu, HL(cpu), HL(cpu)));
      break;
    case 0x63: // LD (nn), HL
      mwrite16(fWord(cpu), HL(cpu));
      break;
    case 0x67: // RRD
      temp1 = memory[HL(cpu)];
      memory[HL(cpu)] = (memory[HL(cpu)] << 4) | (temp1 >> 4);
      cpu->regs[REG_A] = temp1 & 0x0F;
      break;
    case 0x68: // IN L, (C)
      cpu->regs[REG_L] = InputHandler(cpu->regs[REG_C]);
      break;
    case 0x69: // OUT (C), L
      OutputHandler(cpu->regs[REG_C], cpu->regs[REG_L]);
      break;
    case 0x6A: // ADC HL, HL
      set_hl(cpu, adc16(cpu, HL(cpu), HL(cpu)));
      break;
    case 0x6B: // LD HL, (nn)
      set_hl(cpu, mread16(fWord(cpu)));
      break;
    case 0x6F: // RLD
      temp1 = memory[HL(cpu)];
      memory[HL(cpu)] = (temp1 >> 4) | (memory[HL(cpu)] << 4);
      cpu->regs[REG_A] = temp1 & 0x0F;
      break;
    case 0x71: // OUT (C), 0
      OutputHandler(cpu->regs[REG_C], 0);
      break;
    case 0x72: // SBC HL, SP
      set_hl(cpu, sbc16(cpu, HL(cpu), cpu->sp));
      break;
    case 0x73: // LD (nn), SP
      mwrite16(fWord(cpu), cpu->sp);
      break;
    case 0x78: // IN A, (C)
      cpu->regs[REG_A] = InputHandler(cpu->regs[REG_C]);
      break;
    case 0x79: // OUT (C), A
      OutputHandler(cpu->regs[REG_C], cpu->regs[REG_A]);
      break;
    case 0x7A: // ADC HL, SP
      set_hl(cpu, adc16(cpu, HL(cpu), cpu->sp));
      break;
    case 0x7B: // LD SP, (nn)
      cpu->sp = mread16(fWord(cpu));
      break;
    case 0xA0: // LDI
      mwrite8(DE(cpu), mread8(HL(cpu)));
      set_hl(cpu, inc16(HL(cpu))); set_de(cpu, inc16(DE(cpu))); set_bc(cpu, dec16(BC(cpu)));
      if (BC(cpu) == 0) {
        setFlag(cpu, FLAG_PV, 0);
      } else setFlag(cpu, FLAG_PV, 1);
      break;
    case 0xA1: // CPI
      sub8(cpu, cpu->regs[REG_A], mread8(HL(cpu)));
      set_hl(cpu, inc16(HL(cpu))); set_bc(cpu, dec16(BC(cpu)));
      if (BC(cpu) == 0) {
        setFlag(cpu, FLAG_PV, 0);
      } else setFlag(cpu, FLAG_PV, 1);
      break;
    case 0xA2: // INI
      mwrite8(HL(cpu), InputHandler(cpu->regs[REG_C]));
      set_hl(cpu, inc16(HL(cpu))); cpu->regs[REG_B] = dec8(cpu, cpu->regs[REG_B]);
      if (cpu->regs[REG_B] == 0) {
        setFlag(cpu, FLAG_PV, 0);
      } else setFlag(cpu, FLAG_PV, 1);
      break;
    case 0xA3: // OUTI
      OutputHandler(cpu->regs[REG_C], mread8(HL(cpu)));
      set_hl(cpu, inc16(HL(cpu))); cpu->regs[REG_B] = dec8(cpu, cpu->regs[REG_B]);
      if (cpu->regs[REG_B] == 0) {
        setFlag(cpu, FLAG_PV, 0);
      } else setFlag(cpu, FLAG_PV, 1);
      break;
    case 0xA8: // LDD
      mwrite8(DE(cpu), mread8(HL(cpu)));
      set_hl(cpu, dec16(HL(cpu))); set_de(cpu, dec16(DE(cpu))); set_bc(cpu, dec16(BC(cpu)));
      if (BC(cpu) == 0) {
        setFlag(cpu, FLAG_PV, 0);
      } else setFlag(cpu, FLAG_PV, 1);
      break;
    case 0xA9: // CPD
      sub8(cpu, cpu->regs[REG_A], mread8(HL(cpu)));
      set_hl(cpu, dec16(HL(cpu))); set_bc(cpu, dec16(BC(cpu)));
      if (BC(cpu) == 0) {
        setFlag(cpu, FLAG_PV, 0);
      } else setFlag(cpu, FLAG_PV, 1);
      break;
    case 0xAA: // IND
      mwrite8(HL(cpu), InputHandler(cpu->regs[REG_C]));
      set_hl(cpu, dec16(HL(cpu))); cpu->regs[REG_B] = dec8(cpu, cpu->regs[REG_B]);
      if (cpu->regs[REG_B] == 0) {
        setFlag(cpu, FLAG_PV, 0);
      } else setFlag(cpu, FLAG_PV, 1);
      break;
    case 0xAB: // OUTD
      OutputHandler(cpu->regs[REG_C], mread8(HL(cpu)));
      set_hl(cpu, dec16(HL(cpu))); cpu->regs[REG_B] = dec8(cpu, cpu->regs[REG_B]);
      if (cpu->regs[REG_B] == 0) {
        setFlag(cpu, FLAG_PV, 0);
      } else setFlag(cpu, FLAG_PV, 1);
      break;
    case 0xB0: // LDIR
      mwrite8(DE(cpu), mread8(HL(cpu)));
      set_hl(cpu, inc16(HL(cpu))); set_de(cpu, inc16(DE(cpu))); set_bc(cpu, dec16(BC(cpu)));
      if (BC(cpu) != 0) {
        setFlag(cpu, FLAG_PV, 1);
        cpu->pc -= 2;
      } else {
        setFlag(cpu, FLAG_PV, 0);
      }
      break;
    case 0xB1: // CPIR
      sub8(cpu, cpu->regs[REG_A], mread8(HL(cpu)));
      set_hl(cpu, inc16(HL(cpu))); set_bc(cpu, dec16(BC(cpu)));
      if ((BC(cpu) != 0) && !(getFlag(cpu, FLAG_Z))) {
        setFlag(cpu, FLAG_PV, 1);
        cpu->pc -= 2;
      } else {
        setFlag(cpu, FLAG_PV, 0);
      }
      break;
    case 0xB2: // INIR
      mwrite8(HL(cpu), InputHandler(cpu->regs[REG_C]));
      set_hl(cpu, inc16(HL(cpu))); cpu->regs[REG_B] = dec8(cpu, cpu->regs[REG_B]);
      if (cpu->regs[REG_B] != 0) {
        setFlag(cpu, FLAG_PV, 1);
        cpu->pc -= 2;
      } else {
        setFlag(cpu, FLAG_PV, 0);
      }
      break;
    case 0xB3: // OTIR
      OutputHandler(cpu->regs[REG_C], mread8(HL(cpu)));
      set_hl(cpu, inc16(HL(cpu))); cpu->regs[REG_B] = dec8(cpu, cpu->regs[REG_B]);
      if (cpu->regs[REG_B] != 0) {
        setFlag(cpu, FLAG_PV, 1);
        cpu->pc -= 2;
      } else {
        setFlag(cpu, FLAG_PV, 0);
      }
      break;
    case 0xB8: // LDDR
      mwrite8(DE(cpu), mread8(HL(cpu)));
      set_hl(cpu, dec16(HL(cpu))); set_de(cpu, dec16(DE(cpu))); set_bc(cpu, dec16(BC(cpu)));
      if (BC(cpu) != 0) {
        setFlag(cpu, FLAG_PV, 1);
        cpu->pc -= 2;
      } else {
        setFlag(cpu, FLAG_PV, 0);
      }
      break;
    case 0xB9: // CPDR
      sub8(cpu, cpu->regs[REG_A], mread8(HL(cpu)));
      set_hl(cpu, dec16(HL(cpu))); set_bc(cpu, dec16(BC(cpu)));
      if (BC(cpu) != 0 && !(getFlag(cpu, FLAG_Z))) {
        setFlag(cpu, FLAG_PV, 1);
        cpu->pc -= 2;
      } else {
        setFlag(cpu, FLAG_PV, 0);
      }
      break;
    case 0xBA: // INDR
      mwrite8(HL(cpu), InputHandler(cpu->regs[REG_C]));
      set_hl(cpu, dec16(HL(cpu))); cpu->regs[REG_B] = dec8(cpu, cpu->regs[REG_B]);
      if (cpu->regs[REG_B] != 0) {
        setFlag(cpu, FLAG_PV, 1);
        cpu->pc -= 2;
      } else {
        setFlag(cpu, FLAG_PV, 0);
      }
      break;
    case 0xBB: // OTDR
      OutputHandler(cpu->regs[REG_C], mread8(HL(cpu)));
      set_hl(cpu, dec16(HL(cpu))); cpu->regs[REG_B] = dec8(cpu, cpu->regs[REG_B]);
      if (cpu->regs[REG_B] != 0) {
        setFlag(cpu, FLAG_PV, 1);
        cpu->pc -= 2;
      } else {
        setFlag(cpu, FLAG_PV, 0);
      }
      break;
    default:
      printf("Unknown MISC opcode: 0x%02x\n", opcode);
      break;
  }
}

/*
    case ALU_OP_RES:
      result = dest8 & ~(1 << source8);
      break;
    case ALU_OP_SET:
      result = dest8| (1 << source8);
      break;
*/

void BitInstruction(VirtZ80 *cpu) {
  uint8_t opcode = fByte(cpu);
  //if (debug) printf("BitInstruction: 0x%02x\n", opcode);
  switch (opcode) {
    case 0x00: // RLC reg
    case 0x01:
    case 0x02:
    case 0x03:
    case 0x04:
    case 0x05:
    case 0x07:
      cpu->regs[(opcode & 0x07)] = rlc8(cpu, cpu->regs[(opcode & 0x07)]);
      break;
    case 0x06: // RLC (HL)
      mwrite8(HL(cpu), rlc8(cpu, mread8(HL(cpu))));
      break;

    case 0x08: // RRC reg
    case 0x09:
    case 0x0A:
    case 0x0B:
    case 0x0C:
    case 0x0D:
    case 0x0F:
      cpu->regs[((opcode-8) & 0x07)] = rrc8(cpu, cpu->regs[((opcode-8) & 0x07)]);
      break;
    case 0x0E: // RRC (HL)
      mwrite8(HL(cpu), rrc8(cpu, mread8(HL(cpu))));
      break;
    
    case 0x10: // RL reg
    case 0x11:
    case 0x12:
    case 0x13:
    case 0x14:
    case 0x15:
    case 0x17:
      cpu->regs[(opcode & 0x07)] = rl8(cpu, cpu->regs[(opcode & 0x07)]);
      break;
    case 0x16: // RL (HL)
      mwrite8(HL(cpu), rl8(cpu, mread8(HL(cpu))));
      break;

    case 0x18: // RR reg
    case 0x19:
    case 0x1A:
    case 0x1B:
    case 0x1C:
    case 0x1D:
    case 0x1F:
      cpu->regs[((opcode-8) & 0x07)] = rr8(cpu, cpu->regs[((opcode-8) & 0x07)]);
      break;
    case 0x1E: // RR (HL)
      mwrite8(HL(cpu), rr8(cpu, mread8(HL(cpu))));
      break;

    case 0x20: // SLA reg
    case 0x21:
    case 0x22:
    case 0x23:
    case 0x24:
    case 0x25:
    case 0x27:
      cpu->regs[(opcode & 0x07)] = sla8(cpu, cpu->regs[(opcode & 0x07)]);
      break;
    case 0x26: // SLA (HL)
      mwrite8(HL(cpu), sla8(cpu, mread8(HL(cpu))));
      break;

    case 0x28: // SRA reg
    case 0x29:
    case 0x2A:
    case 0x2B:
    case 0x2C:
    case 0x2D:
    case 0x2F:
      cpu->regs[((opcode-8) & 0x07)] = sra8(cpu, cpu->regs[((opcode-8) & 0x07)]);
      break;
    case 0x2E: // SRA (HL)
      mwrite8(HL(cpu), sra8(cpu, mread8(HL(cpu))));
      break;

    case 0x30: // SLL reg
    case 0x31:
    case 0x32:
    case 0x33:
    case 0x34:
    case 0x35:
    case 0x37:
      cpu->regs[(opcode & 0x07)] = sll8(cpu, cpu->regs[(opcode & 0x07)]);
      break;
    case 0x36: // SLL (HL)
      mwrite8(HL(cpu), sll8(cpu, mread8(HL(cpu))));
      break;

    case 0x38: // SRL reg
    case 0x39:
    case 0x3A:
    case 0x3B:
    case 0x3C:
    case 0x3D:
    case 0x3F:
      cpu->regs[((opcode-8) & 0x07)] = srl8(cpu, cpu->regs[((opcode-8) & 0x07)]);
      break;
    case 0x3E: // SRL (HL)
      mwrite8(HL(cpu), srl8(cpu, mread8(HL(cpu))));
      break;
    default:
      break;
  }
  if (opcode >= 0x40 && opcode <= 0x7F) { /* BIT */
    uint8_t bit = (opcode >> 3) & 0x07;
    uint8_t reg = opcode & 0x07;
    
    if (reg == 0x06) { /* (HL)*/
      bit8(cpu, mread8(HL(cpu)), bit);
    } else {
      bit8(cpu, cpu->regs[reg], bit);
    }
  } else if (opcode >= 0x80 && opcode <= 0xBF) { /* RES*/
    uint8_t bit = (opcode >> 3) & 0x07;
    uint8_t reg = opcode & 0x07;
    
    if (reg == 0x06) { /* (HL)*/
      mwrite8(HL(cpu),res8(cpu, mread8(HL(cpu)), bit));
    } else {
      cpu->regs[reg] = res8(cpu, cpu->regs[reg], bit);
    }
  } else if (opcode >= 0xC0 && opcode <= 0xFF) { /* SET*/
    uint8_t bit = (opcode >> 3) & 0x07;
    uint8_t reg = opcode & 0x07;
    
    if (reg == 0x06) { /* (HL)*/
      mwrite8(HL(cpu),set8(cpu, mread8(HL(cpu)), bit));
    } else {
      cpu->regs[reg] = set8(cpu, cpu->regs[reg], bit);
    }
  }
}

void IndexInstruction(VirtZ80 *cpu, uint16_t* index_reg) { // Smart way to do this
  uint8_t opcode = fByte(cpu);
  //if (debug) printf("IX/IY Instruction: 0x%02x\n", opcode);
  switch (opcode) {
    case 0x09: // ADD IX/IY, BC
      *index_reg = add16(cpu, *index_reg, BC(cpu));
      break;
    case 0x19: // ADD IX/IY, DE
      *index_reg = add16(cpu, *index_reg, DE(cpu));
      break;
    case 0x21: // LD IX/IY, nn
      *index_reg = fWord(cpu);
      break;
    case 0x22: // LD (nn), IX/IY
      mwrite16(fWord(cpu), *index_reg);
      break;
    case 0x23: // INC IX/IY
      *index_reg += 1;
      break;
    case 0x2A: // LD IX/IY, (nn)
      *index_reg = mread16(fWord(cpu));
      break;
    case 0x2B: // DEC IX/IY
      *index_reg -= 1;
      break;
    case 0x34: // INC (IX/IY+d)
      cpu->wz = *index_reg + (int8_t)fByte(cpu);
      mwrite8(cpu->wz, inc8(cpu, mread8(cpu->wz)));
      break;
    case 0x35: // DEC (IX/IY+d)
      cpu->wz = *index_reg + (int8_t)fByte(cpu);
      mwrite8(cpu->wz, dec8(cpu, mread8(cpu->wz)));
      break;
    case 0x39: // ADD IX/IY, SP
      *index_reg = add16(cpu, *index_reg, cpu->sp);
      break;

    case 0xE1: // POP IX/IY
      *index_reg = pop(cpu);
      break;
    case 0xE3: // EX (SP), IX/IY
      cpu->wz = *index_reg;
      *index_reg = mread16(cpu->sp);
      mwrite16(cpu->sp, cpu->wz);
      break;
    case 0xE5: // PUSH IX/IY
      push(cpu, *index_reg);
      break;
    case 0xE9: // JP (IX/IY)
      cpu->pc = *index_reg;
      break;
    case 0xF9: // LD SP, IX/IY
      cpu->sp = *index_reg;
      break;
    default:
      printf("Unknown index instruction: 0x%02x at 0x%04x\n", opcode, cpu->pc);
      return;
  }
}
void printState(VirtZ80 *cpu) {
  printf(
    "AF=0x%04x BC=0x%04x DE=0x%04x HL=0x%04x IX=0x%04x IY=0x%04x SP=0x%04x PC=0x%04x\n",
    AF(cpu), BC(cpu), DE(cpu), HL(cpu), cpu->ix, cpu->iy, cpu->sp, cpu->pc
  );
}

void stackTrace(VirtZ80 *cpu, int depth) {
  printf("--STACK TRACE--\n");
  int sp = cpu->sp;
  for (int i = 0; i < depth; i++) {
    if (sp >= 0xFFFF || sp < 0 || (sp+1 >= MEM_SIZE)) break;
    uint16_t value = memory[sp] | (memory[sp+1] << 8);
    printf("0x%04x: 0x%04x\n", sp, value);
    sp += 2;
  }
}

void printMemory(VirtZ80 *cpu) {
  printf("--MEMORY--\n");
  for (int i = 0; i < MEM_SIZE; i += 16) {
    printf("%04x: ", i);
    for (int j = 0; j < 16; j++) {
      printf("%02x ", memory[i + j]);
    }
    printf("\n");
  }
}

int main(int argc, char **argv) {
  if (argc < 2) {
    perror("Usage: ./bemu80 <program>\n");
    exit(1);
  }


  uint16_t program_start = 0x0000;
  uint16_t start_pc = 0x0000;

  bool printmem = false;

  for (int i = 2; i < argc; i++) {
    if (strcmp(argv[i], "-ins") == 0) {
      print_ins = true;
    } else if (strcmp(argv[i], "-pc") == 0) {
      start_pc = strtol(argv[i+1], NULL, 16);
    } else if (strcmp(argv[i], "-org") == 0) {
      program_start = strtol(argv[i+1], NULL, 16);
    } else if (strcmp(argv[i], "-mem") == 0) {
      printmem = true;
    }
  }

  FILE *source = fopen(argv[1], "rb");
  if (source == NULL) {
    perror("fopen");
    exit(1);
  }

  VirtZ80 cpu;
  memset(&cpu, 0, sizeof(VirtZ80));
  fseek(source, 0, SEEK_END);
  long file_size = ftell(source);
  fseek(source, 0, SEEK_SET);

  char *src_hex = (char *)malloc(sizeof(char) * file_size);

  int i = 0;
  while (fread(&src_hex[i], 1, 1, source)) {
    i++;
  }

  fclose(source);

  memset(memory, 0, MEM_SIZE);

  for (int j = 0; j < file_size; j++) {
    memory[j+program_start] = src_hex[j];
  }

  free(src_hex);

  printf("Loaded %d bytes\n", i);

  if (printmem) printMemory(&cpu);

  cpu.pc = start_pc;

  execute(&cpu);
  printState(&cpu);
  //stackTrace(&cpu, 10);
  //printMemory(&cpu);
  return 0;
}
