/* SPDX-License-Identifier: GPL-2.0-or-later
/Copyright (c) 2025 Benjamin Helle
*/ 
#include "BEMU80.H"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

bool print_ins = false;

#include <termios.h>
#include <ncurses.h>

int getch_in(void) {
  struct termios oldt, newt;
  int ch;

  tcgetattr(STDIN_FILENO, &oldt);           // Save old settings
  newt = oldt;
  newt.c_lflag &= ~(ICANON | ECHO);         // Disable buffering and echo
  tcsetattr(STDIN_FILENO, TCSANOW, &newt);  // Apply new settings

  ch = getchar();                           // Read one character

  tcsetattr(STDIN_FILENO, TCSANOW, &oldt);  // Restore old settings
  return ch;
}

void execute(VirtZ80 *cpu) {
  while (!cpu->halt) {
    //if (!stepping) usleep(500); // adjust delay
    cpu->r += 1;
    if (print_ins) printf("Instruction: 0x%02x at 0x%04x | ", cpu->memory[cpu->pc], cpu->pc);
    if (print_ins) printState(cpu);
    MainInstruction(cpu);
  }
}


uint8_t fByte(VirtZ80 *cpu) {
  return cpu->memory[cpu->pc++];
}

int8_t fSByte(VirtZ80 *cpu) {
  return cpu->memory[cpu->pc++];
}

uint16_t fWord(VirtZ80 *cpu) {
  uint16_t result = cpu->memory[cpu->pc++];
  result |= cpu->memory[cpu->pc++] << 8;
  return result;
}


void exchange(VirtZ80 *cpu, uint8_t regpair1, uint8_t regpair2) {
  uint16_t temp1 = getRegpair(cpu, regpair1);
  uint16_t temp2 = getRegpair(cpu, regpair2);
  loadRegpair(cpu, regpair1, temp2);
  loadRegpair(cpu, regpair2, temp1);
}

void exchange_shadow(VirtZ80 *cpu, uint8_t regpair, uint8_t shadowregpair) {
  uint16_t shadow_reg = getShadowRegpair(cpu, shadowregpair);
  uint16_t reg = getRegpair(cpu, regpair);
  loadShadowRegpair(cpu, shadowregpair, reg);
  loadRegpair(cpu, regpair, shadow_reg);
}

void push_new(VirtZ80 *cpu, uint8_t high, uint8_t low) {
  cpu->memory[--cpu->sp] = high;
  cpu->memory[--cpu->sp] = low;
}

void pop_new(VirtZ80 *cpu, uint8_t* high, uint8_t* low) {
  *low = cpu->memory[cpu->sp++];
  *high = cpu->memory[cpu->sp++];
  //cpu->sp += 2;
}

void push(VirtZ80 *cpu, uint16_t value) {
  cpu->memory[--cpu->sp] = value >> 8;
  cpu->memory[--cpu->sp] = value & 0xFF;
}

uint16_t pop(VirtZ80 *cpu) {
  uint16_t result = cpu->memory[cpu->sp++];
  result |= cpu->memory[cpu->sp++] << 8;
  return result;
}

void loadRegpair(VirtZ80 *cpu, uint8_t regpair, uint16_t value) {
  if (regpair == REG_AF) { // special case
    cpu->regs[REG_A] = value >> 8;
    cpu->flags = value & 0xFF;
  } else {
    cpu->regs[regpair] = value >> 8;
    cpu->regs[regpair + 1] = value & 0xFF;
  }
}

void loadShadowRegpair(VirtZ80 *cpu, uint8_t regpair, uint16_t value) {
  if (regpair == REG_AF) { // special case
    cpu->shadow_regs[REG_A] = value >> 8;
    cpu->shadow_flags = value & 0xFF;
  } else {
    cpu->shadow_regs[regpair] = value >> 8;
    cpu->shadow_regs[regpair + 1] = value & 0xFF;
  }
}

uint16_t getRegpair(VirtZ80 *cpu, uint8_t regpair) {
  uint16_t value = 0;
  if (regpair == REG_AF) { // special case
    value = cpu->regs[REG_A] << 8 | cpu->flags;
  } else {
    value = cpu->regs[regpair] << 8 | cpu->regs[regpair + 1];
  }
  return value;
}

uint16_t getShadowRegpair(VirtZ80 *cpu, uint8_t regpair) {
  uint16_t value = 0;
  if (regpair == REG_AF) { // special case
    value = cpu->shadow_regs[REG_A] << 8 | cpu->shadow_flags;
  } else {
    value = cpu->shadow_regs[regpair] << 8 | cpu->shadow_regs[regpair + 1];
  }
  return value;
}

void update_flags8(VirtZ80 *cpu, uint16_t alu_result, uint8_t flags) {
  
}
/*void updateFlags8(VirtZ80 *cpu, uint16_t alu_result) {
  cpu->flags = (cpu->flags & ~(FLAG_Z | FLAG_S | FLAG_C | FLAG_H | FLAG_P)) |
            (alu_result == 0 ? FLAG_Z : 0x00) |
            (alu_result & 0x80 ? FLAG_S : 0x00) |
            (alu_result & 0x10 ? FLAG_H : 0x00) |
            (alu_result & 0x100 ? FLAG_C : 0x00) |
            ((alu_result & 0x0F) + (cpu->flags & FLAG_C ? 1 : 0) > 0xF ? FLAG_P : 0x00);
}*/

void updateFlags8Rotate(VirtZ80 *cpu, uint16_t alu_result) {
  cpu->flags = (cpu->flags & ~(FLAG_Z | FLAG_S | FLAG_H | FLAG_P)) |
            (alu_result == 0 ? FLAG_Z : 0x00) |
            (alu_result & 0x80 ? FLAG_S : 0x00) |
            (alu_result & 0x10 ? FLAG_H : 0x00) |
            ((alu_result & 0x0F) + (cpu->flags & FLAG_C ? 1 : 0) > 0xF ? FLAG_P : 0x00);
}

void updateFlags16(VirtZ80 *cpu, uint32_t alu_result) {
  cpu->flags = (cpu->flags & ~(FLAG_Z | FLAG_S | FLAG_C | FLAG_H | FLAG_P)) |
            (alu_result == 0 ? FLAG_Z : 0x00) |
            (alu_result & 0x8000 ? FLAG_S : 0x00) |
            (alu_result & 0x10000 ? FLAG_C : 0x00) |
            (alu_result & 0x100 ? FLAG_H : 0x00) |
            (alu_result < -32768 || alu_result > 32767 ? FLAG_P : 0x00);
}


// NEW FLAGS, TODO: ADD THEM

void flag_update8(VirtZ80 *cpu, uint8_t flags, uint16_t alu_result) { // change defined flags, eg. 0x41 changes zero and carry
}

void uf_c8(VirtZ80 *cpu, uint16_t alu_result) { // update carry 8-bit
  if (alu_result & 0x100) cpu->flags |= FLAG_C;
  else cpu->flags &= ~FLAG_C;
}

void uf_c16(VirtZ80 *cpu, uint32_t alu_result) { // update carry 16-bit
  if (alu_result & 0x10000) cpu->flags |= FLAG_C;
  else cpu->flags &= ~FLAG_C;
}

void uf_z(VirtZ80 *cpu, uint16_t alu_result) { // update zero flag
  if (alu_result == 0) cpu->flags |= FLAG_Z;
  else cpu->flags &= ~FLAG_Z;
}

void uf_s8(VirtZ80 *cpu, uint8_t alu_result) { // update sign flag 8-bit
  if (alu_result & 0x80) cpu->flags |= FLAG_S;
  else cpu->flags &= ~FLAG_S;
}

void uf_s16(VirtZ80 *cpu, uint16_t alu_result) { // update sign flag 16-bit
  if (alu_result & 0x8000) cpu->flags |= FLAG_S;
  else cpu->flags &= ~FLAG_S;
}

void uf_p8(VirtZ80 *cpu, uint8_t alu_result) { // update parity flag 8-bit
  if ((alu_result & 0x0F) + (cpu->flags & FLAG_C ? 1 : 0) > 0xF) cpu->flags |= FLAG_P;
  else cpu->flags &= ~FLAG_P;
}

void uf_p16(VirtZ80 *cpu, uint16_t alu_result) { // update parity flag 16-bit
  if ((alu_result & 0x0F) + (cpu->flags & FLAG_C ? 1 : 0) > 0xF) cpu->flags |= FLAG_P;
  else cpu->flags &= ~FLAG_P;
}

void uf_o8(VirtZ80 *cpu, uint16_t alu_result) { // update overflow flag 8-bit
  if (alu_result >= 0xFF) cpu->flags |= FLAG_H;
  else cpu->flags &= ~FLAG_H;
}

void uf_o16(VirtZ80 *cpu, uint32_t alu_result) { // update overflow flag 16-bit
  if (alu_result >= 0xFFFF) cpu->flags |= FLAG_H;
  else cpu->flags &= ~FLAG_H;
}


uint8_t getFlag(VirtZ80 *cpu, uint8_t flag) {
  return (cpu->flags & flag) ? 1 : 0;
}

void setFlag(VirtZ80 *cpu, uint8_t flag, uint8_t value) {
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
  if (port == STD_PORT) input = getch_in(); // STDIN
  //printf("ASCII CODE: %02x", input);
  return input;
}

// New alu8
void alu8(VirtZ80 *cpu, uint8_t *dest, uint8_t src, uint8_t ins) {
  int16_t result = 0;
  uint8_t dest8 = *dest;
  uint8_t source8 = src;
  switch (ins) {
    case ALU_OP_ADD:
      result = dest8 + source8;
      update_flags8(cpu, result, 0xFF);
      setFlag(cpu, FLAG_N, 0); /* Reset N flag */
      break;
    case ALU_OP_ADC:
      result = dest8 + source8 + getFlag(cpu, FLAG_C);
      update_flags8(cpu, result, 0xFF);
      setFlag(cpu, FLAG_N, 0);
      break;
    case ALU_OP_SUB:
      result = dest8 - source8;
      update_flags8(cpu, result, 0xFF);
      setFlag(cpu, FLAG_N, 1); /* Set N flag */
      break;
    case ALU_OP_SBC:
      result = dest8 - source8 - getFlag(cpu, FLAG_C);
      update_flags8(cpu, result, 0xFF);
      setFlag(cpu, FLAG_N, 1);
      break;
    case ALU_OP_AND:
      result = dest8 & source8;
      update_flags8(cpu, result, FLAG_P | FLAG_Z | FLAG_S);
      setFlag(cpu, FLAG_C | FLAG_H, 0);
      setFlag(cpu, FLAG_H, 1);
      break;
    case ALU_OP_OR:
      result = dest8 | source8;
      update_flags8(cpu, result, FLAG_P | FLAG_Z | FLAG_S);
      setFlag(cpu, FLAG_C | FLAG_N | FLAG_H, 0);
      break;
    case ALU_OP_XOR:
      result = dest8 ^ source8;
      update_flags8(cpu, result, FLAG_P | FLAG_Z | FLAG_S);
      setFlag(cpu, FLAG_C | FLAG_N | FLAG_H, 0);
      break;
    case ALU_OP_CP:
      result = dest8 - source8;
      update_flags8(cpu, result, 0xFF);
      setFlag(cpu, FLAG_N, 1);
      return;
    case ALU_OP_INC:
      result = dest8 + 1;
      break;
    case ALU_OP_DEC:
      result = dest8 - 1;
      break;

    case ALU_OP_RLC:
      result = (dest8<< 1) | (dest8 >> 7);
      if (dest8 & 0x80) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;
      break;
    case ALU_OP_RRC:
      result = (dest8 >> 1) | (dest8 << 7);
      if (dest8 & 0x01) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;
      break;
    case ALU_OP_RL:
    {
      uint8_t carry = getFlag(cpu, FLAG_C);
      result = (dest8 << 1) | carry;
      if (dest8& 0x80) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;
      break;
    }
    case ALU_OP_RR:
    {
      uint8_t carry = getFlag(cpu, FLAG_C);
      result = (dest8 >> 1) | (carry << 7);
      if (dest8 & 0x01) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;
      break;
    }
    case ALU_OP_SLA:
      result = (dest8 << 1);
      if (dest8 & 0x80) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;
      break;
    case ALU_OP_SRA:
      result = (dest8 >> 1) | (dest8 & 0x80);
      if (dest8 & 0x01) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;
      break;
    case ALU_OP_SRL:
      result = (dest8 >> 1);
      if (dest8 & 0x01) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;
      break;
    case ALU_OP_BIT:
      result = dest8 & (1 << source8);
      updateFlags8(cpu, result);
      return;
    case ALU_OP_RES:
      result = dest8 & ~(1 << source8);
      break;
    case ALU_OP_SET:
      result = dest8| (1 << source8);
      break;

    default:
      return;
  }

  *dest = (result & 0xFF);
}
  

/*void alu8(VirtZ80 *cpu, uint8_t dest, uint8_t src, bool is_imm, uint8_t ins) {
  int16_t result = 0;
  uint8_t dest8 = cpu->regs[dest];
  uint8_t source8 = 0;
  if (is_imm) source8 = src;
  else source8 = cpu->regs[src];
  switch (ins) {
    case ALU_OP_ADD:
      result = dest8 + source8;
      break;
    case ALU_OP_ADC:
      result = dest8 + source8 + getFlag(cpu, FLAG_C);
      break;
    case ALU_OP_SUB:
      result = dest8 - source8;
      break;
    case ALU_OP_SBC:
      result = dest8 - source8 - getFlag(cpu, FLAG_C);
      break;
    case ALU_OP_AND:
      result = dest8 & source8;
      break;
    case ALU_OP_OR:
      result = dest8 | source8;
      break;
    case ALU_OP_XOR:
      result = dest8 ^ source8;
      break;
    case ALU_OP_CP:
      result = dest8 - source8;
      updateFlags8(cpu, result);
      return;
    case ALU_OP_INC:
      result = dest8 + 1;
      break;
    case ALU_OP_DEC:
      result = dest8 - 1;
      break;

    case ALU_OP_RLC:
      result = (dest8<< 1) | (dest8 >> 7);
      if (dest8 & 0x80) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;

      cpu ->regs[dest] = (result & 0xFF);
      updateFlags8Rotate(cpu, result);
      return;
    case ALU_OP_RRC:
      result = (dest8 >> 1) | (dest8 << 7);
      if (dest8 & 0x01) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;

      cpu ->regs[dest] = (result & 0xFF);
      updateFlags8Rotate(cpu, result);
      return;
    case ALU_OP_RL:
    {
      uint8_t carry = getFlag(cpu, FLAG_C);
      result = (dest8 << 1) | carry;
      if (dest8& 0x80) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;
      cpu ->regs[dest] = (result & 0xFF);
      updateFlags8Rotate(cpu, result);
      return;
    }
    case ALU_OP_RR:
    {
      uint8_t carry = getFlag(cpu, FLAG_C);
      result = (dest8 >> 1) | (carry << 7);
      if (dest8 & 0x01) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;

      cpu ->regs[dest] = (result & 0xFF);
      updateFlags8Rotate(cpu, result);
      return;
    }
    case ALU_OP_SLA:
      result = (dest8 << 1);
      if (dest8 & 0x80) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;

      cpu ->regs[dest] = (result & 0xFF);
      updateFlags8Rotate(cpu, result);
      return;
    case ALU_OP_SRA:
      result = (dest8 >> 1) | (dest8 & 0x80);
      if (dest8 & 0x01) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;

      cpu ->regs[dest] = (result & 0xFF);
      updateFlags8Rotate(cpu, result);
      return;
    case ALU_OP_SRL:
      result = (dest8 >> 1);
      if (dest8 & 0x01) cpu->flags |= FLAG_C;
      else cpu->flags &= ~FLAG_C;

      cpu ->regs[dest] = (result & 0xFF);
      updateFlags8Rotate(cpu, result);
      return;

    case ALU_OP_BIT:
      result = dest8 & (1 << source8);
      updateFlags8(cpu, result);
      return;
    case ALU_OP_RES:
      result = dest8 & ~(1 << source8);
      break;
    case ALU_OP_SET:
      result = dest8| (1 << source8);
      break;

    default:
      return;
  }

  updateFlags8(cpu, result);
  cpu ->regs[dest] = (result & 0xFF);
}*/

// TODO: Fix all alu flags

// No immediate on 16-bit ALU
void alu16(VirtZ80 *cpu, uint8_t dest, uint8_t src, uint8_t ins) {
  int32_t result = 0;
  uint16_t dest16 = getRegpair(cpu, dest);
  uint16_t source16 = getRegpair(cpu, src);
  switch (ins) {
    case ALU_OP_ADD:
      result = dest16 + source16;
      break;
    case ALU_OP_ADC:
      result = dest16 + source16 + getFlag(cpu, FLAG_C);
      break;
    case ALU_OP_SUB:
      result = dest16 - source16;
      break;
    case ALU_OP_SBC:
      result = dest16 - source16 - getFlag(cpu, FLAG_C);
      break;
    case ALU_OP_INC:
      result = dest16 + 1;
      loadRegpair(cpu, dest, (result & 0xFFFF));
      return;
    case ALU_OP_DEC:
      result = (dest16 - 1);
      loadRegpair(cpu, dest, (result & 0xFFFF));
      return;

    default:
      return;
  }

  updateFlags16(cpu, result);
  loadRegpair(cpu, dest, (result & 0xFFFF));
}

void alu16_imm(VirtZ80 *cpu, uint16_t* dest, uint16_t value, uint8_t ins) {
  if (!dest) {
    return;
  }
  int32_t result = 0;
  switch (ins) {
    case ALU_OP_ADD:
      result = *dest + value;
      break;
    case ALU_OP_ADC:
      result = *dest + value + getFlag(cpu, FLAG_C);
      break;
    case ALU_OP_SUB:
      result = *dest - value;
      break;
    case ALU_OP_SBC:
      result = *dest - value - getFlag(cpu, FLAG_C);
      break;
    case ALU_OP_INC:
      result = *dest + 1;
      return;
    case ALU_OP_DEC:
      result = *dest - 1;
      return;

    default:
      return;
  }

  updateFlags16(cpu, result);
  *dest = (result & 0xFFFF);
}

void MainInstruction(VirtZ80 *cpu) {
  uint8_t opcode = fByte(cpu);
  uint16_t addr = 0;
  int8_t reladdr = 0;
  uint8_t temp1 = 0;
  switch (opcode) {
    case 0x00: // NOP
      break;
    case 0x01: // LD BC, nn
      loadRegpair(cpu, REG_BC, fWord(cpu));
      break;
    case 0x02: // LD (BC), A
      cpu->memory[getRegpair(cpu, REG_BC)] = cpu->regs[REG_A];
      break;
    case 0x03: // INC BC
      alu16(cpu, REG_BC, REG_BC, ALU_OP_INC);
      break;
    case 0x04: // INC B
      //alu8(cpu, REG_B, REG_B, false, ALU_OP_INC);
      update_flags8(cpu, ++cpu->regs[REG_B], 0xFE);
      break;
    case 0x05: // DEC B
      //alu8(cpu, REG_B, REG_B, false, ALU_OP_DEC);
      update_flags8(cpu, --cpu->regs[REG_B], 0xFE);
      break;
    case 0x06: // LD B, n
      cpu->regs[REG_B] = fByte(cpu);
      break;
    case 0x07: // RLCA
      alu8(cpu, REG_A, REG_A, 0, ALU_OP_RLC);
      break;
    case 0x08: // EX AF, AF'
      exchange_shadow(cpu, REG_AF, REG_AF);
      break;
    case 0x09: // ADD HL, BC
      alu16(cpu, REG_HL, REG_BC, ALU_OP_ADD);
      break;
    case 0x0A: // LD A, (BC)
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_BC)];
      break;
    case 0x0B: // DEC BC
      alu16(cpu, REG_BC, REG_BC, ALU_OP_DEC);
      break;
    case 0x0C: // INC C
      alu8(cpu, REG_C, REG_C, false, ALU_OP_INC);
      break;
    case 0x0D: // DEC C
      alu8(cpu, REG_C, REG_C, false, ALU_OP_DEC);
      break;
    case 0x0E: // LD C, n
      cpu->regs[REG_C] = fByte(cpu);
      break;
    case 0x0F: // RRCA
      alu8(cpu, REG_A, REG_A, 0, ALU_OP_RRC);
      break;
    case 0x10: // DJNZ d
      alu8(cpu, REG_B, REG_B, false, ALU_OP_DEC);
      reladdr = (int8_t)fByte(cpu);
      if (cpu->regs[REG_B] != 0) {
        cpu->pc += reladdr;
      }
      break;
    case 0x11: // LD DE, nn
      loadRegpair(cpu, REG_DE, fWord(cpu));
      break;
    case 0x12: // LD (DE), A
      cpu->memory[getRegpair(cpu, REG_DE)] = cpu->regs[REG_A];
      break;
    case 0x13: // INC DE
      alu16(cpu, REG_DE, REG_DE, ALU_OP_INC);
      break;
    case 0x14: // INC D
      alu8(cpu, REG_D, REG_D, false, ALU_OP_INC);
      break;
    case 0x15: // DEC D
      alu8(cpu, REG_B, REG_B, false, ALU_OP_DEC);
      break;
    case 0x16: // LD D, n
      cpu->regs[REG_D] = fByte(cpu);
      break;
    case 0x17: // RLA
      alu8(cpu, REG_A, REG_A, false, ALU_OP_RL);
      break;
    case 0x18: // JR d
      cpu->pc += (int8_t)fByte(cpu);
      break;
    case 0x19: // ADD HL, DE
      alu16(cpu, REG_HL, REG_DE, ALU_OP_ADD);
      break;
    case 0x1A: // LD A, (DE)
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_DE)];
      break;
    case 0x1B: // DEC DE
      alu16(cpu, REG_DE, REG_DE, ALU_OP_DEC);
      break;
    case 0x1C: // INC E
      alu8(cpu, REG_E, REG_E, false, ALU_OP_INC);
      break;
    case 0x1D: // DEC E
      alu8(cpu, REG_E, REG_E, false, ALU_OP_DEC);
      break;
    case 0x1E: // LD E, n
      cpu->regs[REG_E] = fByte(cpu);
      break;
    case 0x1F: // RRA
      alu8(cpu, REG_A, REG_A, false, ALU_OP_RR);
      break;
    case 0x20: // JR NZ, d
      reladdr = (int8_t)fByte(cpu);
      if (getFlag(cpu, FLAG_Z) == 0) {
        cpu->pc += reladdr;
      }
      break;
    case 0x21: // LD HL, nn
      loadRegpair(cpu, REG_HL, fWord(cpu));
      break;
    case 0x22: // LD (nn), HL
      cpu->wz = fWord(cpu);
      cpu->memory[cpu->wz] = cpu->regs[REG_L];
      cpu->memory[cpu->wz + 1] = cpu->regs[REG_H];
      break;
    case 0x23: // INC HL
      alu16(cpu, REG_HL, REG_HL, ALU_OP_INC);
      break;
    case 0x24: // INC H
      alu8(cpu, REG_H, REG_H, false, ALU_OP_INC);
      break;
    case 0x25: // DEC H
      alu8(cpu, REG_H, REG_H, false, ALU_OP_DEC);
      break;
    case 0x26: // LD H, n
      cpu->regs[REG_H] = fByte(cpu);
      break;
    case 0x27: // DAA
      // TODO
      break;
    case 0x28: // JR Z, d
      reladdr = fSByte(cpu);
      if (getFlag(cpu, FLAG_Z) == 1) {
        cpu->pc += reladdr;
      }
      break;
    case 0x29: // ADD HL, HL
      alu16(cpu, REG_HL, REG_HL, ALU_OP_ADD);
      break;
    case 0x2A: // LD HL, (nn)
      cpu->wz = fWord(cpu);
      cpu->regs[REG_L] = cpu->memory[cpu->wz];
      cpu->regs[REG_H] = cpu->memory[cpu->wz + 1];
      break;
    case 0x2B: // DEC HL
      alu16(cpu, REG_HL, REG_HL, ALU_OP_DEC);
      break;
    case 0x2C: // INC L
      alu8(cpu, REG_L, REG_L, false, ALU_OP_INC);
      break;
    case 0x2D: // DEC L
      alu8(cpu, REG_L, REG_L, false, ALU_OP_DEC);
      break;
    case 0x2E: // LD L, n
      cpu->regs[REG_L] = fByte(cpu);
      break;
    case 0x2F: // CPL
      cpu->regs[REG_A] = ~cpu->regs[REG_A];
      setFlag(cpu, FLAG_N, 1);
      setFlag(cpu, FLAG_H, 1);
      break;
    case 0x30: // JR NC, d
      reladdr = fSByte(cpu);
      if (getFlag(cpu, FLAG_C) == 0) {
        cpu->pc += (int8_t)reladdr;
        break;
      }
      break;
    case 0x31: // LD SP, nn
      cpu->sp = fWord(cpu);
      break;
    case 0x32: // LD (nn), A
      cpu->memory[fWord(cpu)] = cpu->regs[REG_A];
      break;
    case 0x33: // INC SP
      cpu->sp += 1;
      break;
    case 0x34: // INC (HL)
      temp1 = cpu->regs[REG_A];
      cpu->wz = getRegpair(cpu, REG_HL);
      cpu->regs[REG_A] = cpu->memory[cpu->wz];
      alu8(cpu, REG_A, REG_A, false, ALU_OP_INC);
      cpu->memory[cpu->wz] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;
    case 0x35: // DEC (HL)
      temp1 = cpu->regs[REG_A];
      cpu->wz = getRegpair(cpu, REG_HL);
      cpu->regs[REG_A] = cpu->memory[cpu->wz];
      alu8(cpu, REG_A, REG_A, false, ALU_OP_DEC);
      cpu->memory[cpu->wz] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;
    case 0x36: // LD (HL), n
      cpu->memory[getRegpair(cpu, REG_HL)] = fByte(cpu);
      break;
    case 0x37: // SCF
      setFlag(cpu, FLAG_C, 1);
      break;
    case 0x38: // JR C, d
      reladdr = fSByte(cpu);
      if (getFlag(cpu, FLAG_C) == 1) {
        cpu->pc += (int8_t)reladdr;
      }
      break;
    case 0x39: // ADD HL, SP
      cpu->wz = getRegpair(cpu, REG_HL); // Use addr for temp
      alu16_imm(cpu, &cpu->wz, cpu->sp, ALU_OP_ADD);
      loadRegpair(cpu, REG_HL, cpu->wz);
      break;
    case 0x3A: // LD A, (nn)
      cpu->wz = fWord(cpu);
      cpu->regs[REG_A] = cpu->memory[cpu->wz];
      break;
    case 0x3B: // DEC SP
      cpu->sp -= 1;
      break;
    case 0x3C: // INC A
      alu8(cpu, REG_A, REG_A, false, ALU_OP_INC);
      break;
    case 0x3D: // DEC A
      alu8(cpu, REG_A, REG_A, false, ALU_OP_DEC);
      break;
    case 0x3E: // LD A, n
      cpu->regs[REG_A] = fByte(cpu);
      break;
    case 0x3F: // CCF
      setFlag(cpu, FLAG_C, getFlag(cpu, FLAG_C) ^ 1);
      break;
    case 0x40: // LD B, B
      break;
    case 0x41: // LD B, reg
    case 0x42: 
    case 0x43:
    case 0x44:
    case 0x45:
    case 0x47:
      cpu->regs[REG_B] = cpu->regs[opcode & 0x07];
      break;
    case 0x46: // LD B, (HL)
      cpu->regs[REG_B] = cpu->memory[getRegpair(cpu, REG_HL)];
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
      cpu->regs[REG_C] = cpu->memory[getRegpair(cpu, REG_HL)];
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
      cpu->regs[REG_D] = cpu->memory[getRegpair(cpu, REG_HL)];
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
      cpu->regs[REG_E] = cpu->memory[getRegpair(cpu, REG_HL)];
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
      cpu->regs[REG_H] = cpu->memory[getRegpair(cpu, REG_HL)];
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
      cpu->regs[REG_L] = cpu->memory[getRegpair(cpu, REG_HL)];
      break;

    case 0x70: // LD (HL), reg
    case 0x71:
    case 0x72:
    case 0x73:
    case 0x74:
    case 0x75:
    case 0x77:
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[opcode & 0x07];
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
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      break;

    case 0x80: // ADD A, reg
    case 0x81:
    case 0x82:
    case 0x83:
    case 0x84:
    case 0x85:
    case 0x87:
      alu8(cpu, REG_A, (opcode & 0x07), false, ALU_OP_ADD);
      break;
    case 0x86: // ADD A, (HL)
      alu8(cpu, REG_A, cpu->memory[getRegpair(cpu, REG_HL)], true, ALU_OP_ADD);
      break;

    case 0x88: // ADC A, reg
    case 0x89:
    case 0x8A:
    case 0x8B:
    case 0x8C:
    case 0x8D:
    case 0x8F:
      alu8(cpu, REG_A, ((opcode-8) & 0x07), false, ALU_OP_ADC);
      break;
    case 0x8E: // ADC A, (HL)
      alu8(cpu, REG_A, cpu->memory[getRegpair(cpu, REG_HL)], true, ALU_OP_ADC);
      break;

    case 0x90: // SUB A, reg
    case 0x91:
    case 0x92:
    case 0x93:
    case 0x94:
    case 0x95:
    case 0x97:
      alu8(cpu, REG_A, (opcode & 0x07), false, ALU_OP_SUB);
      break;
    case 0x96: // SUB A, (HL)
      alu8(cpu, REG_A, cpu->memory[getRegpair(cpu, REG_HL)], true, ALU_OP_SUB);
      break;

    case 0x98: // SBC A, reg
    case 0x99:
    case 0x9A:
    case 0x9B:
    case 0x9C:
    case 0x9D:
    case 0x9F:
      alu8(cpu, REG_A, ((opcode-8) & 0x07), false, ALU_OP_SBC);
      break;
    case 0x9E: // SBC A, (HL)
      alu8(cpu, REG_A, cpu->memory[getRegpair(cpu, REG_HL)], true, ALU_OP_SBC);
      break;

    case 0xA0: // AND A, reg
    case 0xA1:
    case 0xA2:
    case 0xA3:
    case 0xA4:
    case 0xA5:
    case 0xA7:
      alu8(cpu, REG_A, (opcode & 0x07), false, ALU_OP_AND);
      break;
    case 0xA6: // AND A, (HL)
      alu8(cpu, REG_A, cpu->memory[getRegpair(cpu, REG_HL)], true, ALU_OP_AND);
      break;

    case 0xA8: // XOR A, reg
    case 0xA9:
    case 0xAA:
    case 0xAB:
    case 0xAC:
    case 0xAD:
    case 0xAF:
      alu8(cpu, REG_A, ((opcode-8) & 0x07), false, ALU_OP_XOR);
      break;
    case 0xAE: // XOR A, (HL)
      alu8(cpu, REG_A, cpu->memory[getRegpair(cpu, REG_HL)], true, ALU_OP_XOR);
      break;

    case 0xB0: // OR A, reg
    case 0xB1:
    case 0xB2:
    case 0xB3:
    case 0xB4:
    case 0xB5:
    case 0xB7:
      alu8(cpu, REG_A, (opcode & 0x07), false, ALU_OP_OR);
      break;
    case 0xB6: // OR A, (HL)
      alu8(cpu, REG_A, cpu->memory[getRegpair(cpu, REG_HL)], true, ALU_OP_OR);
      break;

    case 0xB8: // CP A, reg
    case 0xB9:
    case 0xBA:
    case 0xBB:
    case 0xBC:
    case 0xBD:
    case 0xBF:
      alu8(cpu, REG_A, ((opcode-8) & 0x07), false, ALU_OP_CP);
      break;
    case 0xBE: // CP A, (HL)
      alu8(cpu, REG_A, cpu->memory[getRegpair(cpu, REG_HL)], true, ALU_OP_CP);
      break;
    
    case 0xC0: // RET NZ
      if (getFlag(cpu, FLAG_Z) == 0) {
        cpu->pc = pop(cpu);
      }
      break;
    case 0xC1: // POP BC
      pop_new(cpu, &cpu->regs[REG_B], &cpu->regs[REG_C]);
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
      push_new(cpu, cpu->regs[REG_B], cpu->regs[REG_C]);
      break;
    case 0xC6: // ADD A, n
      alu8(cpu, REG_A, fByte(cpu), true, ALU_OP_ADD);
      break;
    case 0xC7: // RST 0h
      push(cpu, cpu->pc);
      cpu->pc = 0x00;
      break;
    case 0xC8: // RET Z
      if (getFlag(cpu, FLAG_Z) == 1) {
        cpu->pc = pop(cpu);
        break;
      }
      break;
    case 0xC9: // RET
      cpu->pc = pop(cpu);
      break;
    case 0xCA: // JP Z, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_Z) != 0) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xCB: // Bit instruction
      BitInstruction(cpu);
      break;
    case 0xCC: // CALL Z, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_Z) == 1) {
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
      alu8(cpu, REG_A, fByte(cpu), true, ALU_OP_ADC);
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
      pop_new(cpu, &cpu->regs[REG_D], &cpu->regs[REG_E]);
      break;
    case 0xD2: // JP NC, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_C) == 0) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xD3: // OUT (n), A
      //printf("\n");
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
      push_new(cpu, cpu->regs[REG_D], cpu->regs[REG_E]);
      break;
    case 0xD6: // SUB A, n
      alu8(cpu, REG_A, fByte(cpu), true, ALU_OP_SUB);
      break;
    case 0xD7: // RST 10h
      push(cpu, cpu->pc);
      cpu->pc = 0x10;
      break;
    case 0xD8: // RET C
      if (getFlag(cpu, FLAG_C) == 1) {
        cpu->pc = pop(cpu);
      }
      break;
    case 0xD9: // EXX
      exchange_shadow(cpu, REG_BC, REG_BC);
      exchange_shadow(cpu, REG_DE, REG_DE);
      exchange_shadow(cpu, REG_HL, REG_HL);
      break;
    case 0xDA: // JP C, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_C) != 0) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xDB: // IN A, (n)
      cpu->regs[REG_A] = InputHandler(fByte(cpu));
      break;
    case 0xDC: // CALL C, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_C) != 0) {
        push(cpu, cpu->pc);
        cpu->pc = cpu->wz;
      }
      break;
    case 0xDD: // IX Prefix
      IndexInstruction(cpu, &cpu->ix);
      break;
    case 0xDE: // SBC A, n
      alu8(cpu, REG_A, fByte(cpu), true, ALU_OP_SBC);
      break;
    case 0xDF: // RST 18h
      push(cpu, cpu->pc);
      cpu->pc = 0x18;
      break;
    case 0xE0: // RET PO
      if (getFlag(cpu, FLAG_P) == 0) {
        cpu->pc = pop(cpu);
      }
      break;
    case 0xE1: // POP HL
      pop_new(cpu, &cpu->regs[REG_H], &cpu->regs[REG_L]);
      break;
    case 0xE2: // JP PO, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_P) == 0) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xE3: // EX (SP),HL
      cpu->wz = getRegpair(cpu, REG_HL);
      cpu->regs[REG_L] = cpu->memory[cpu->sp];
      cpu->regs[REG_H] = cpu->memory[cpu->sp + 1];
      cpu->memory[cpu->sp] = cpu->wz & 0xFF;
      cpu->memory[cpu->sp + 1] = cpu->wz >> 8;
      break;
    case 0xE4: // CALL PO, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_P) == 0) {
        push(cpu, cpu->pc);
        cpu->pc = cpu->wz;
      }
      break;
    case 0xE5: // PUSH HL
      push_new(cpu, cpu->regs[REG_H], cpu->regs[REG_L]);
      break;
    case 0xE6: // AND A, n
      alu8(cpu, REG_A, fByte(cpu), true, ALU_OP_AND);
      break;
    case 0xE7: // RST 20h
      push(cpu, cpu->pc);
      cpu->pc = 0x20;
      break;
    case 0xE8: // RET PE
      if (getFlag(cpu, FLAG_P) == 1) {
        cpu->pc = pop(cpu);
      }
      break;
    case 0xE9: // JP (HL)
      cpu->pc = getRegpair(cpu, REG_HL);
      break;
    case 0xEA: // JP PE, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_P) != 0) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xEB: // EX DE,HL
      exchange(cpu, REG_DE, REG_HL);
      break;
    case 0xEC: // CALL PE, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_P) != 0) {
        push(cpu, cpu->pc);
        cpu->pc = cpu->wz;
      }
      break;
    case 0xED: // Misc. Instructions
      MiscInstruction(cpu);
      break;
    case 0xEE: // XOR A, n
      alu8(cpu, REG_A, fByte(cpu), true, ALU_OP_XOR);
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
      pop_new(cpu, &cpu->regs[REG_A], &cpu->flags);
      break;
    case 0xF2: // JP P, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_P) == 0) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xF3: // DI
      cpu->im = 0;
      break;
    case 0xF4: // CALL P, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_P) == 0) {
        push(cpu, cpu->pc);
        cpu->pc = cpu->wz;
      }
      break;
    case 0xF5: // PUSH AF
      push_new(cpu, cpu->regs[REG_A], cpu->flags);
      break;
    case 0xF6: // OR A, n
      alu8(cpu, REG_A, fByte(cpu), true, ALU_OP_OR);
      break;
    case 0xF7: // RST 30h
      push(cpu, cpu->pc);
      cpu->pc = 0x30;
      break;
    case 0xF8: // RET M
      if (getFlag(cpu, FLAG_S) == 1) {
        cpu->pc = pop(cpu);
      }
      break;
    case 0xF9: // LD SP, HL
      cpu->sp = getRegpair(cpu, REG_HL);
      break;
    case 0xFA: // JP M, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_S) != 0) {
        cpu->pc = cpu->wz;
      }
      break;
    case 0xFB: // EI
      cpu->im = 1;
      break;
    case 0xFC: // CALL M, nn
      cpu->wz = fWord(cpu);
      if (getFlag(cpu, FLAG_S) != 0) {
        push(cpu, cpu->pc);
        cpu->pc = cpu->wz;
      }
      break;
    case 0xFD: // IY Prefix
      IndexInstruction(cpu, &cpu->iy);
      break;
    case 0xFE: // CP A, n
      alu8(cpu, REG_A, fByte(cpu), true, ALU_OP_CP);
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
  uint16_t addr = 0;
  if (print_ins) printf("0xED Instruction: 0x%02x\n", opcode);
  switch (opcode) {
    case 0x40: // IN B, (C)
      cpu->regs[REG_B] = InputHandler(cpu->regs[REG_C]);
      break;
    case 0x41: // OUT (C), B
      OutputHandler(cpu->regs[REG_C], cpu->regs[REG_B]);
      break;
    case 0x42: // SBC HL, BC
      alu16(cpu, REG_HL, REG_BC, ALU_OP_SBC);
      break;
    case 0x43: // LD (nn), BC
      cpu->wz = fWord(cpu);
      cpu->memory[cpu->wz] = cpu->regs[REG_C];
      cpu->memory[cpu->wz + 1] = cpu->regs[REG_B];
      break;
    case 0x44: // NEG
      // Check if working!!
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = 0;
      alu8(cpu, REG_A, temp1, true, ALU_OP_SUB);
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
      alu16(cpu, REG_HL, REG_BC, ALU_OP_ADC);
      break;
    case 0x4B: // LD BC, (nn)
      cpu->wz = fWord(cpu);
      cpu->regs[REG_C] = cpu->memory[cpu->wz];
      cpu->regs[REG_B] = cpu->memory[cpu->wz + 1];
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
      alu16(cpu, REG_HL, REG_DE, ALU_OP_SBC);
      break;
    case 0x53: // LD (nn), DE
      cpu->wz = fWord(cpu);
      cpu->memory[cpu->wz] = cpu->regs[REG_E];
      cpu->memory[cpu->wz + 1] = cpu->regs[REG_D];
      break;
    case 0x56: // IM 1
      cpu->im = 1;
      break;
    case 0x57: // LD A, I
      cpu->regs[REG_A] = cpu->i;
      break;
    case 0x58: // IN E, (C)
      cpu->regs[REG_E] = InputHandler(cpu->regs[REG_C]);
      break;
    case 0x59: // OUT (C), E
      OutputHandler(cpu->regs[REG_C], cpu->regs[REG_E]);
      break;
    case 0x5A: // ADC HL, DE
      alu16(cpu, REG_HL, REG_DE, ALU_OP_ADC);
      break;
    case 0x5B: // LD DE, (nn)
      cpu->wz = fWord(cpu);
      cpu->regs[REG_E] = cpu->memory[cpu->wz];
      cpu->regs[REG_D] = cpu->memory[cpu->wz + 1];
      break;
    case 0x5E: // IM 2
      cpu->im = 2;
      break;
    case 0x5F: // LD A, R
      cpu->regs[REG_A] = cpu->r;
      break;
    case 0x60: // IN H, (C)
      cpu->regs[REG_H] = InputHandler(cpu->regs[REG_C]);
      break;
    case 0x61: // OUT (C), H
      OutputHandler(cpu->regs[REG_C], cpu->regs[REG_H]);
      break;
    case 0x62: // SBC HL, HL
      alu16(cpu, REG_HL, REG_HL, ALU_OP_SBC);
      break;
    case 0x63: // LD (nn), HL
      cpu->wz = fWord(cpu);
      cpu->memory[cpu->wz] = cpu->regs[REG_L];
      cpu->memory[cpu->wz + 1] = cpu->regs[REG_H];
      break;
    case 0x67: // RRD
      temp1 = cpu->memory[getRegpair(cpu, REG_HL)];
      cpu->memory[getRegpair(cpu, REG_HL)] = (cpu->memory[getRegpair(cpu, REG_HL)] << 4) | (temp1 >> 4);
      cpu->regs[REG_A] = temp1 & 0x0F;
      break;
    case 0x68: // IN L, (C)
      cpu->regs[REG_L] = InputHandler(cpu->regs[REG_C]);
      break;
    case 0x69: // OUT (C), L
      OutputHandler(cpu->regs[REG_C], cpu->regs[REG_L]);
      break;
    case 0x6A: // ADC HL, HL
      alu16(cpu, REG_HL, REG_HL, ALU_OP_ADC);
      break;
    case 0x6B: // LD HL, (nn)
      cpu->wz = fWord(cpu);
      cpu->regs[REG_L] = cpu->memory[cpu->wz];
      cpu->regs[REG_H] = cpu->memory[cpu->wz + 1];
      break;
    case 0x6F: // RLD
      temp1 = cpu->memory[getRegpair(cpu, REG_HL)];
      cpu->memory[getRegpair(cpu, REG_HL)] = (temp1 >> 4) | (cpu->memory[getRegpair(cpu, REG_HL)] << 4);
      cpu->regs[REG_A] = temp1 & 0x0F;
      break;
    case 0x71: // OUT (C), 0
      OutputHandler(cpu->regs[REG_C], 0);
      break;
    case 0x72: // SBC HL, SP
      cpu->wz = getRegpair(cpu, REG_HL);
      alu16_imm(cpu, &cpu->wz, cpu->sp, ALU_OP_SBC);
      loadRegpair(cpu, REG_HL, cpu->wz);
      break;
    case 0x73: // LD (nn), SP
      cpu->wz = fWord(cpu);
      cpu->memory[cpu->wz] = (cpu->sp & 0xFF);
      cpu->memory[cpu->wz + 1] = (cpu->sp >> 8) & 0xFF;
      break;
    case 0x78: // IN A, (C)
      cpu->regs[REG_A] = InputHandler(cpu->regs[REG_C]);
      break;
    case 0x79: // OUT (C), A
      OutputHandler(cpu->regs[REG_C], cpu->regs[REG_A]);
      break;
    case 0x7A: // ADC HL, SP
      cpu->wz = getRegpair(cpu, REG_HL);
      alu16_imm(cpu, &cpu->wz, cpu->sp, ALU_OP_ADC);
      loadRegpair(cpu, REG_HL, cpu->wz);
      break;
    case 0x7B: // LD SP, (nn)
      cpu->wz = fWord(cpu);
      cpu->sp = cpu->memory[cpu->wz];
      cpu->sp |= cpu->memory[cpu->wz + 1] << 8;
      break;
    case 0xA0: // LDI
      cpu->memory[getRegpair(cpu, REG_DE)] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu16(cpu, REG_HL, REG_HL, ALU_OP_INC);
      alu16(cpu, REG_DE, REG_DE, ALU_OP_INC);
      alu16(cpu, REG_BC, REG_BC, ALU_OP_DEC);
      if (getRegpair(cpu, REG_BC) == 0) {
        setFlag(cpu, FLAG_P, 0);
      } setFlag(cpu, FLAG_P, 1);
      break;
    case 0xA1: // CPI
      alu8(cpu, REG_A, cpu->memory[getRegpair(cpu, REG_HL)], true, ALU_OP_CP);
      alu16(cpu, REG_HL, REG_HL, ALU_OP_INC);
      alu16(cpu, REG_BC, REG_BC, ALU_OP_DEC);
      if (getRegpair(cpu, REG_BC) == 0) {
        setFlag(cpu, FLAG_P, 0);
      } setFlag(cpu, FLAG_P, 1);
      break;
    case 0xA2: // INI
      cpu->memory[getRegpair(cpu, REG_HL)] = InputHandler(cpu->regs[REG_C]);
      alu16(cpu, REG_HL, REG_HL, ALU_OP_INC);
      alu16(cpu, REG_BC, REG_BC, ALU_OP_DEC);
      if (getRegpair(cpu, REG_BC) == 0) {
        setFlag(cpu, FLAG_P, 0);
      } setFlag(cpu, FLAG_P, 1);
      break;
    case 0xA3: // OUTI
      OutputHandler(cpu->regs[REG_C], cpu->memory[getRegpair(cpu, REG_HL)]);
      alu16(cpu, REG_HL, REG_HL, ALU_OP_INC);
      alu16(cpu, REG_BC, REG_BC, ALU_OP_DEC);
      if (getRegpair(cpu, REG_BC) == 0) {
        setFlag(cpu, FLAG_P, 0);
      } setFlag(cpu, FLAG_P, 1);
      break;
    case 0xA8: // LDD
      cpu->memory[getRegpair(cpu, REG_DE)] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu16(cpu, REG_HL, REG_HL, ALU_OP_DEC);
      alu16(cpu, REG_DE, REG_DE, ALU_OP_DEC);
      alu16(cpu, REG_BC, REG_BC, ALU_OP_DEC);
      if (getRegpair(cpu, REG_BC) == 0) {
        setFlag(cpu, FLAG_P, 0);
      } setFlag(cpu, FLAG_P, 1);
      break;
    case 0xA9: // CPD
      alu8(cpu, REG_A, cpu->memory[getRegpair(cpu, REG_HL)], true, ALU_OP_CP);
      alu16(cpu, REG_HL, REG_HL, ALU_OP_DEC);
      alu16(cpu, REG_BC, REG_BC, ALU_OP_DEC);
      if (getRegpair(cpu, REG_BC) == 0) {
        setFlag(cpu, FLAG_P, 0);
      } setFlag(cpu, FLAG_P, 1);
      break;
    case 0xAA: // IND
      cpu->memory[getRegpair(cpu, REG_HL)] = InputHandler(cpu->regs[REG_C]);
      alu16(cpu, REG_HL, REG_HL, ALU_OP_DEC);
      alu16(cpu, REG_BC, REG_BC, ALU_OP_DEC);
      if (getRegpair(cpu, REG_BC) == 0) {
        setFlag(cpu, FLAG_P, 0);
      } setFlag(cpu, FLAG_P, 1);
      break;
    case 0xAB: // OUTD
      OutputHandler(cpu->regs[REG_C], cpu->memory[getRegpair(cpu, REG_HL)]);
      alu16(cpu, REG_HL, REG_HL, ALU_OP_DEC);
      alu16(cpu, REG_BC, REG_BC, ALU_OP_DEC);
      if (getRegpair(cpu, REG_BC) == 0) {
        setFlag(cpu, FLAG_P, 0);
      } setFlag(cpu, FLAG_P, 1);
      break;
    case 0xB0: // LDIR
      cpu->memory[getRegpair(cpu, REG_DE)] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu16(cpu, REG_HL, REG_HL, ALU_OP_INC);
      alu16(cpu, REG_DE, REG_DE, ALU_OP_INC);
      alu16(cpu, REG_BC, REG_BC, ALU_OP_DEC);
      if (getRegpair(cpu, REG_BC) != 0) {
        setFlag(cpu, FLAG_P, 1);
        cpu->pc -= 2;
      } else {
         setFlag(cpu, FLAG_P, 0);
      }
      break;
    case 0xB1: // CPIR
      alu8(cpu, REG_A, cpu->memory[getRegpair(cpu, REG_HL)], true, ALU_OP_CP);
      alu16(cpu, REG_HL, REG_HL, ALU_OP_INC);
      alu16(cpu, REG_BC, REG_BC, ALU_OP_DEC);
      if (getRegpair(cpu, REG_BC) != 0 && !(getFlag(cpu, FLAG_Z))) {
        setFlag(cpu, FLAG_P, 1);
        cpu->pc -= 2;
      } else {
        cpu->flags &= ~FLAG_P;
      }
      break;
    case 0xB2: // INIR
      cpu->memory[getRegpair(cpu, REG_HL)] = InputHandler(cpu->regs[REG_C]);
      alu8(cpu, REG_B, REG_B, false, ALU_OP_DEC);
      alu16(cpu, REG_HL, REG_HL, ALU_OP_INC);
      if (cpu->regs[REG_B] != 0) {
        setFlag(cpu, FLAG_P, 1);
        cpu->pc -= 2;
      } else {
        cpu->flags &= ~FLAG_P;
      }
      break;
    case 0xB3: // OTIR
      OutputHandler(cpu->regs[REG_C], cpu->memory[getRegpair(cpu, REG_HL)]);
      alu8(cpu, REG_B, REG_B, false, ALU_OP_DEC);
      alu16(cpu, REG_HL, REG_HL, ALU_OP_INC);
      if (cpu->regs[REG_B] != 0) {
        setFlag(cpu, FLAG_P, 1);
        cpu->pc -= 2;
      } else {
        cpu->flags &= ~FLAG_P;
      }
      break;
    case 0xB8: // LDDR
      cpu->memory[getRegpair(cpu, REG_DE)] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu16(cpu, REG_HL, REG_HL, ALU_OP_DEC);
      alu16(cpu, REG_DE, REG_DE, ALU_OP_DEC);
      alu16(cpu, REG_BC, REG_BC, ALU_OP_DEC);
      if (getRegpair(cpu, REG_BC) != 0) {
        setFlag(cpu, FLAG_P, 1);
        cpu->pc -= 2;
      } else {
         setFlag(cpu, FLAG_P, 0);
      }
      break;
    case 0xB9: // CPDR
      alu8(cpu, REG_A, cpu->memory[getRegpair(cpu, REG_HL)], true, ALU_OP_CP);
      alu16(cpu, REG_HL, REG_HL, ALU_OP_DEC);
      alu16(cpu, REG_BC, REG_BC, ALU_OP_DEC);
      if (getRegpair(cpu, REG_BC) != 0 && !(getFlag(cpu, FLAG_Z))) {
        setFlag(cpu, FLAG_P, 1);
        cpu->pc -= 2;
      } else {
        cpu->flags &= ~FLAG_P;
      }
      break;
    case 0xBA: // INDR
      cpu->memory[getRegpair(cpu, REG_HL)] = InputHandler(cpu->regs[REG_C]);
      alu8(cpu, REG_B, REG_B, false, ALU_OP_DEC);
      alu16(cpu, REG_HL, REG_HL, ALU_OP_DEC);
      if (cpu->regs[REG_B] != 0) {
        setFlag(cpu, FLAG_P, 1);
        cpu->pc -= 2;
      } else {
        cpu->flags &= ~FLAG_P;
      }
      break;
    case 0xBB: // OTDR
      OutputHandler(cpu->regs[REG_C], cpu->memory[getRegpair(cpu, REG_HL)]);
      alu8(cpu, REG_B, REG_B, false, ALU_OP_DEC);
      alu16(cpu, REG_HL, REG_HL, ALU_OP_DEC);
      if (cpu->regs[REG_B] != 0) {
        setFlag(cpu, FLAG_P, 1);
        cpu->pc -= 2;
      } else {
        cpu->flags &= ~FLAG_P;
      }
      break;
    default:
      printf("Unknown MISC opcode: 0x%02x\n", opcode);
      break;
  }
}

void BitInstruction(VirtZ80 *cpu) {
  uint8_t opcode = fByte(cpu);
  uint8_t temp1 = 0;
  //if (debug) printf("BitInstruction: 0x%02x\n", opcode);
  switch (opcode) {
    case 0x00: // RLC reg
    case 0x01:
    case 0x02:
    case 0x03:
    case 0x04:
    case 0x05:
    case 0x07:
      alu8(cpu, (opcode & 0x07), 0, false, ALU_OP_RLC);
      break;
    case 0x06: // RLC (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 0, false, ALU_OP_RLC);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0x08: // RRC reg
    case 0x09:
    case 0x0A:
    case 0x0B:
    case 0x0C:
    case 0x0D:
    case 0x0F:
      alu8(cpu, ((opcode-8) & 0x07), 0, false, ALU_OP_RRC);
      break;
    case 0x0E: // RRC (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 0, false, ALU_OP_RRC);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;
    
    case 0x10: // RL reg
    case 0x11:
    case 0x12:
    case 0x13:
    case 0x14:
    case 0x15:
    case 0x17:
      alu8(cpu, (opcode & 0x07), 0, false, ALU_OP_RL);
      break;
    case 0x16: // RL (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 0, false, ALU_OP_RL);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0x18: // RR reg
    case 0x19:
    case 0x1A:
    case 0x1B:
    case 0x1C:
    case 0x1D:
    case 0x1F:
      alu8(cpu, ((opcode-8) & 0x07), 0, false, ALU_OP_RR);
      break;
    case 0x1E: // RR (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 0, false, ALU_OP_RR);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0x20: // SLA reg
    case 0x21:
    case 0x22:
    case 0x23:
    case 0x24:
    case 0x25:
    case 0x27:
      alu8(cpu, (opcode & 0x07), 0, false, ALU_OP_SLA);
      break;
    case 0x26: // SLA (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 0, false, ALU_OP_SLA);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0x28: // SRA reg
    case 0x29:
    case 0x2A:
    case 0x2B:
    case 0x2C:
    case 0x2D:
    case 0x2F:
      alu8(cpu, ((opcode-8) & 0x07), 0, false, ALU_OP_SRA);
      break;
    case 0x2E: // SRA (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 0, false, ALU_OP_SRA);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0x30: // SLL reg, TODO
    case 0x31:
    case 0x32:
    case 0x33:
    case 0x34:
    case 0x35:
    case 0x37:
      //alu8(cpu, (opcode & 0x07), 0, false, ALU_OP_SLL);
      break;
    case 0x36: // SLL (HL)
      /*temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, &cpu->regs[REG_A], 0, false, ALU_OP_SLL);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;*/
      break;

    case 0x38: // SRL reg
    case 0x39:
    case 0x3A:
    case 0x3B:
    case 0x3C:
    case 0x3D:
    case 0x3F:
      alu8(cpu, ((opcode-8) & 0x07), 0, false, ALU_OP_SRL);
      break;
    case 0x3E: // SRL (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 0, false, ALU_OP_SRL);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;
    
    case 0x40: // BIT 0, reg
    case 0x41:
    case 0x42:
    case 0x43:
    case 0x44:
    case 0x45:
    case 0x47:
      alu8(cpu, (opcode & 0x07), 0, false, ALU_OP_BIT);
      break;
    case 0x46: // BIT 0, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 0, false, ALU_OP_BIT);
      cpu->regs[REG_A] = temp1;
      break;
    
    case 0x48: // BIT 1, reg
    case 0x49:
    case 0x4A:
    case 0x4B:
    case 0x4C:
    case 0x4D:
    case 0x4F:
      alu8(cpu, ((opcode-8) & 0x07), 1, false, ALU_OP_BIT);
      break;
    case 0x4E: // BIT 1, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 1, false, ALU_OP_BIT);
      cpu->regs[REG_A] = temp1;
      break;
    
    case 0x50: // BIT 2, reg
    case 0x51:
    case 0x52:
    case 0x53:
    case 0x54:
    case 0x55:
    case 0x57:
      alu8(cpu, (opcode & 0x07), 2, false, ALU_OP_BIT);
      break;
    case 0x56: // BIT 2, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 2, false, ALU_OP_BIT);
      cpu->regs[REG_A] = temp1;
      break;
    
    case 0x58: // BIT 3, reg
    case 0x59:
    case 0x5A:
    case 0x5B:
    case 0x5C:
    case 0x5D:
    case 0x5F:
      alu8(cpu, ((opcode-8) & 0x07), 3, false, ALU_OP_BIT);
      break;
    case 0x5E: // BIT 3, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 3, false, ALU_OP_BIT);
      cpu->regs[REG_A] = temp1;
      break;
    
    case 0x60: // BIT 4, reg
    case 0x61:
    case 0x62:
    case 0x63:
    case 0x64:
    case 0x65:
    case 0x67:
      alu8(cpu, (opcode & 0x07), 4, false, ALU_OP_BIT);
      break;
    case 0x66: // BIT 4, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 4, false, ALU_OP_BIT);
      cpu->regs[REG_A] = temp1;
      break;
    
    case 0x68: // BIT 5, reg
    case 0x69:
    case 0x6A:
    case 0x6B:
    case 0x6C:
    case 0x6D:
    case 0x6F:
      alu8(cpu, ((opcode-8) & 0x07), 5, false, ALU_OP_BIT);
      break;
    case 0x6E: // BIT 5, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 5, false, ALU_OP_BIT);
      cpu->regs[REG_A] = temp1;
      break;
    
    case 0x70: // BIT 6, reg
    case 0x71:
    case 0x72:
    case 0x73:
    case 0x74:
    case 0x75:
    case 0x77:
      alu8(cpu, (opcode & 0x07), 6, false, ALU_OP_BIT);
      break;
    case 0x76: // BIT 6, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 6, false, ALU_OP_BIT);
      cpu->regs[REG_A] = temp1;
      break;
    
    case 0x78: // BIT 7, reg
    case 0x79:
    case 0x7A:
    case 0x7B:
    case 0x7C:
    case 0x7D:
    case 0x7F:
      alu8(cpu, ((opcode-8) & 0x07), 7, false, ALU_OP_BIT);
      break;
    case 0x7E: // BIT 7, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 7, false, ALU_OP_BIT);
      cpu->regs[REG_A] = temp1;
      break;

    case 0x80: // RES 0, reg
    case 0x81:
    case 0x82:
    case 0x83:
    case 0x84:
    case 0x85:
    case 0x87:
      alu8(cpu, (opcode & 0x07), 0, false, ALU_OP_RES);
      break;
    case 0x86: // RES 0, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 0, false, ALU_OP_RES);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0x88: // RES 1, reg
    case 0x89:
    case 0x8A:
    case 0x8B:
    case 0x8C:
    case 0x8D:
    case 0x8F:
      alu8(cpu, ((opcode-8) & 0x07), 1, false, ALU_OP_RES);
      break;
    case 0x8E: // RES 1, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 1, false, ALU_OP_RES);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0x90: // RES 2, reg
    case 0x91:
    case 0x92:
    case 0x93:
    case 0x94:
    case 0x95:
    case 0x97:
      alu8(cpu, (opcode & 0x07), 2, false, ALU_OP_RES);
      break;
    case 0x96: // RES 2, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 2, false, ALU_OP_RES);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0x98: // RES 3, reg
    case 0x99:
    case 0x9A:
    case 0x9B:
    case 0x9C:
    case 0x9D:
    case 0x9F:
      alu8(cpu, ((opcode-8) & 0x07), 3, false, ALU_OP_RES);
      break;
    case 0x9E: // RES 3, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 3, false, ALU_OP_RES);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0xA0: // RES 4, reg
    case 0xA1:
    case 0xA2:
    case 0xA3:
    case 0xA4:
    case 0xA5:
    case 0xA7:
      alu8(cpu, (opcode & 0x07), 4, false, ALU_OP_RES);
      break;
    case 0xA6: // RES 4, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 4, false, ALU_OP_RES);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0xA8: // RES 5, reg
    case 0xA9:
    case 0xAA:
    case 0xAB:
    case 0xAC:
    case 0xAD:
    case 0xAF:
      alu8(cpu, ((opcode-8) & 0x07), 5, false, ALU_OP_RES);
      break;
    case 0xAE: // RES 5, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 5, false, ALU_OP_RES);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;
    
    case 0xB0: // RES 6, reg
    case 0xB1:
    case 0xB2:
    case 0xB3:
    case 0xB4:
    case 0xB5:
    case 0xB7:
      alu8(cpu, (opcode & 0x07), 6, false, ALU_OP_RES);
      break;
    case 0xB6: // RES 6, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 6, false, ALU_OP_RES);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;
    
    case 0xB8: // RES 7, reg
    case 0xB9:
    case 0xBA:
    case 0xBB:
    case 0xBC:
    case 0xBD:
    case 0xBF:
      alu8(cpu, ((opcode-8) & 0x07), 7, false, ALU_OP_RES);
      break;
    case 0xBE: // RES 7, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 7, false, ALU_OP_RES);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0xC0: // SET 0, reg
    case 0xC1:
    case 0xC2:
    case 0xC3:
    case 0xC4:
    case 0xC5:
    case 0xC7:
      alu8(cpu, (opcode & 0x07), 0, false, ALU_OP_SET);
      break;
    case 0xC6: // SET 0, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 0, false, ALU_OP_SET);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0xC8: // SET 1, reg
    case 0xC9:
    case 0xCA:
    case 0xCB:
    case 0xCC:
    case 0xCD:
    case 0xCF:
      alu8(cpu, ((opcode-8) & 0x07), 1, false, ALU_OP_SET);
      break;
    case 0xCE: // SET 1, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 1, false, ALU_OP_SET);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0xD0: // SET 2, reg
    case 0xD1:
    case 0xD2:
    case 0xD3:
    case 0xD4:
    case 0xD5:
    case 0xD7:
      alu8(cpu, (opcode & 0x07), 2, false, ALU_OP_SET);
      break;
    case 0xD6: // SET 2, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 2, false, ALU_OP_SET);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0xD8: // SET 3, reg
    case 0xD9:
    case 0xDA:
    case 0xDB:
    case 0xDC:
    case 0xDD:
    case 0xDF:
      alu8(cpu, ((opcode-8) & 0x07), 3, false, ALU_OP_SET);
      break;
    case 0xDE: // SET 3, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 3, false, ALU_OP_SET);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0xE0: // SET 4, reg
    case 0xE1:
    case 0xE2:
    case 0xE3:
    case 0xE4:
    case 0xE5:
    case 0xE7:
      alu8(cpu, (opcode & 0x07), 4, false, ALU_OP_SET);
      break;
    case 0xE6: // SET 4, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 4, false, ALU_OP_SET);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0xE8: // SET 5, reg
    case 0xE9:
    case 0xEA:
    case 0xEB:
    case 0xEC:
    case 0xED:
    case 0xEF:
      alu8(cpu, ((opcode-8) & 0x07), 5, false, ALU_OP_SET);
      break;
    case 0xEE: // SET 5, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 5, false, ALU_OP_SET);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0xF0: // SET 6, reg
    case 0xF1:
    case 0xF2:
    case 0xF3:
    case 0xF4:
    case 0xF5:
    case 0xF7:
      alu8(cpu, (opcode & 0x07), 6, false, ALU_OP_SET);
      break;
    case 0xF6: // SET 6, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 6, false, ALU_OP_SET);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    case 0xF8: // SET 7, reg
    case 0xF9:
    case 0xFA:
    case 0xFB:
    case 0xFC:
    case 0xFD:
    case 0xFF:
      alu8(cpu, ((opcode-8) & 0x07), 7, false, ALU_OP_SET);
      break;
    case 0xFE: // SET 7, (HL)
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[getRegpair(cpu, REG_HL)];
      alu8(cpu, REG_A, 7, false, ALU_OP_SET);
      cpu->memory[getRegpair(cpu, REG_HL)] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;

    default:
      printf("Unknown BIT instruction: 0x%02x at 0x%04x", opcode, cpu->pc);
      return;
  }   
}

void IndexInstruction(VirtZ80 *cpu, uint16_t* index_reg) { // Smart way to do this
  uint8_t opcode = fByte(cpu);
  uint8_t temp1;
  uint16_t addr;
  //if (debug) printf("IX/IY Instruction: 0x%02x\n", opcode);
  switch (opcode) {
    case 0x09: // ADD IX/IY, BC
      alu16_imm(cpu, index_reg, getRegpair(cpu, REG_BC), ALU_OP_ADD);
      break;
    case 0x19: // ADD IX/IY, DE
      alu16_imm(cpu, index_reg, getRegpair(cpu, REG_DE), ALU_OP_ADD);
      break;
    case 0x21: // LD IX/IY, nn
      *index_reg = fWord(cpu);
      break;
    case 0x22: // LD (nn), IX/IY
      cpu->wz = fWord(cpu);
      cpu->memory[cpu->wz] = *index_reg & 0xFF;
      cpu->memory[cpu->wz + 1] = *index_reg >> 8;
      break;
    case 0x23: // INC IX/IY
      *index_reg += 1;
      break;
    case 0x2A: // LD IX/IY, (nn)
      cpu->wz = fWord(cpu);
      *index_reg = cpu->memory[cpu->wz] | (cpu->memory[cpu->wz + 1] << 8);
      break;
    case 0x2B: // DEC IX/IY
      *index_reg -= 1;
      break;
    case 0x34: // INC (IX/IY+d)
      cpu->wz = *index_reg + (int8_t)fByte(cpu);
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[cpu->wz];
      alu8(cpu, REG_A, 0, false, ALU_OP_INC);
      cpu->memory[cpu->wz] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;
    case 0x35: // DEC (IX/IY+d)
      cpu->wz = *index_reg + (int8_t)fByte(cpu);
      temp1 = cpu->regs[REG_A];
      cpu->regs[REG_A] = cpu->memory[cpu->wz];
      alu8(cpu, REG_A, 0, false, ALU_OP_DEC);
      cpu->memory[cpu->wz] = cpu->regs[REG_A];
      cpu->regs[REG_A] = temp1;
      break;
    case 0x39: // ADD IX/, SP
      alu16_imm(cpu, index_reg, cpu->sp, ALU_OP_ADD);
      break;

    case 0xE1: // POP IX/IY
      *index_reg = pop(cpu);
      break;
    case 0xE3: // EX (SP), IX/IY
      cpu->wz = *index_reg;
      *index_reg = cpu->memory[cpu->sp];
      *index_reg = cpu->memory[cpu->sp + 1] << 8;
      cpu->memory[cpu->sp] = cpu->wz & 0xFF;
      cpu->memory[cpu->sp + 1] = cpu->wz >> 8;
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
    getRegpair(cpu, REG_AF), getRegpair(cpu, REG_BC), getRegpair(cpu, REG_DE), getRegpair(cpu, REG_HL), cpu->ix, cpu->iy, cpu->sp, cpu->pc
  );
}

void stackTrace(VirtZ80 *cpu, int depth) {
  printf("--STACK TRACE--\n");
  int sp = cpu->sp;
  for (int i = 0; i < depth; i++) {
    uint16_t value = cpu->memory[sp] | (cpu->memory[sp+1] << 8);
    printf("0x%04x: 0x%04x\n", sp, value);
    sp += 2;
    if (sp >= 0x10000) break;
  }
}

void printMemory(VirtZ80 *cpu) {
  printf("--MEMORY--\n");
  int addr = 0;
  for (int i = 0; i < MEM_SIZE; i += 16) {
    printf("%04x: ", i);
    for (int j = 0; j < 16; j++) {
      printf("%02x ", cpu->memory[i + j]);
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

  for (int i = 2; i < argc; i++) {
    if (strcmp(argv[i], "-ins") == 0) {
      print_ins = true;
    } else if (strcmp(argv[i], "-pc") == 0) {
      start_pc = strtol(argv[i+1], NULL, 16);
    } else if (strcmp(argv[i], "-org") == 0) {
      program_start = strtol(argv[i+1], NULL, 16);
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

  memset(&cpu.memory, 0, MEM_SIZE);

  for (int j = 0; j < file_size; j++) {
    cpu.memory[j+program_start] = src_hex[j];
  }

  printf("Loaded %d bytes\n", i);

  cpu.sp = 0xfffe;

  execute(&cpu);
  printState(&cpu);
  stackTrace(&cpu, 10);
  //printMemory(&cpu);
  return 0;
}
