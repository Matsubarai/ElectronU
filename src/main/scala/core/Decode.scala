package core

import chisel3._
import chisel3.util._
import Instructions._
import ALU._
import MulDiv._
import BranchCompare._

class CtrlSignals extends Bundle{
  val alu_ctrl = UInt(SZ_ALU_CTRL.W)
  val muldiv_ctrl = UInt(SZ_MULDIV_CTRL.W)
  val rj = UInt(5.W)
  val rk = UInt(5.W)
  val rd = UInt(5.W)
  val sel_src = UInt(6.W)
  val mem2reg = Bool()
  val reg2mem = Bool()
  val bhw = UInt(3.W)
  val br_comp_ctrl = UInt(SZ_B_CTRL.W)
  val b = Bool()
  val bl = Bool()
  val jirl = Bool()
  val brk = Bool()
  val sys = Bool()
  val csr = Bool()
  val imm26 = UInt(26.W)
}

class Decode extends Module {
  val io = IO(new Bundle() {
    val instr = Input(UInt(32.W))
    val ctrl = Valid(new CtrlSignals)
  })

  io.ctrl.valid := Seq(ADD_W, ADDI_W, SUB_W,
    LD_BU, LD_HU, LD_B, LD_H, LD_W, ST_W, ST_H, ST_B,
    BEQ, BNE, BLT, BGE, BLTU, BGEU, JIRL, B, BL,
    SLT, SLTU, SLTUI, SLTI,
    SLL_W, SRL_W, SRA_W, SLLI_W, SRLI_W, SRAI_W,
    MUL_W, MULH_W, MULH_WU, DIV_W, DIV_WU, MOD_W, MOD_WU,
    BREAK, SYSCALL, LU12I_W, PCADDU12I,
    NOR, AND, OR, XOR, ANDI, ORI, XORI).map(io.instr === _).reduce(_ || _)

  io.ctrl.bits.rj := io.instr(9, 5)
  io.ctrl.bits.rk := io.instr(14, 10)
  io.ctrl.bits.rd := io.instr(4, 0)
  io.ctrl.bits.imm26 := io.instr(25, 0)

  io.ctrl.bits.alu_ctrl := Mux1H(Cat(Seq(io.instr === ADD_W || io.instr === ADDI_W ||
    io.instr === LD_W || io.instr === ST_W || io.instr === LD_BU || io.instr === LD_B || io.instr === ST_B ||
    io.instr === LD_HU || io.instr === LD_H || io.instr === ST_H || io.instr === JIRL || io.instr === BL ||
    io.instr === PCADDU12I || io.instr === LU12I_W, io.instr === SUB_W, io.instr === SLT || io.instr === SLTI,
    io.instr === SLTU || io.instr === SLTUI, io.instr === SLL_W || io.instr === SLLI_W,
    io.instr === SRL_W || io.instr === SRLI_W, io.instr === SRA_W || io.instr === SRAI_W,
    io.instr === NOR, io.instr === AND, io.instr === OR, io.instr === XOR).reverse),
    Seq(ALU_ADD, ALU_SUB, ALU_SLT, ALU_SLTU, ALU_SLL, ALU_SRL, ALU_SRA, ALU_NOR, ALU_AND, ALU_OR, ALU_XOR))

  io.ctrl.bits.muldiv_ctrl := Cat(io.instr === DIV_W || io.instr === MOD_W || io.instr === DIV_WU || io.instr === MOD_WU,
    io.instr === MULH_WU || io.instr === MULH_W || io.instr === DIV_W || io.instr === DIV_WU,
    io.instr === MULH_W || io.instr === MUL_W || io.instr === DIV_W || io.instr === MOD_W)

  io.ctrl.bits.sel_src := Cat(io.instr === LU12I_W, io.instr === PCADDU12I,
    io.instr === JIRL || io.instr === BL,
    io.instr === SLLI_W || io.instr === SRLI_W || io.instr === SRAI_W,
    io.instr === ANDI || io.instr === ORI || io.instr === XORI,
    io.instr === ADDI_W || io.instr === SLTI || io.instr === SLTUI ||
      io.instr === LD_BU || io.instr === LD_B || io.instr === ST_B ||
      io.instr === LD_HU || io.instr === LD_H || io.instr === ST_H ||
      io.instr === LD_W || io.instr === ST_W )

  io.ctrl.bits.mem2reg := io.instr === LD_W || io.instr === LD_B || io.instr === LD_H ||
    io.instr === LD_BU || io.instr === LD_HU

  io.ctrl.bits.reg2mem := io.instr === ST_W || io.instr === ST_B || io.instr === ST_H

  io.ctrl.bits.bhw := Cat(io.instr === LD_B || io.instr === LD_H,
    io.instr === LD_W || io.instr === ST_W,
    io.instr === LD_H || io.instr === LD_HU || io.instr === ST_H)

  io.ctrl.bits.br_comp_ctrl := Cat(io.instr === BNE || io.instr === BGE || io.instr === BGEU,
    io.instr === BLT || io.instr === BGE || io.instr === BLTU || io.instr === BGEU,
    io.instr === BEQ || io.instr === BNE || io.instr === BLT || io.instr === BGE)

  io.ctrl.bits.b := io.instr === B

  io.ctrl.bits.bl := io.instr === BL

  io.ctrl.bits.jirl := io.instr === JIRL

  io.ctrl.bits.brk := io.instr === BREAK

  io.ctrl.bits.sys := io.instr === SYSCALL

  io.ctrl.bits.csr := io.instr === CSR

}