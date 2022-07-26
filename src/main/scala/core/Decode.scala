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
  val sel_src = UInt(5.W)
  val mem2reg = Bool()
  val reg2mem = Bool()
  val bhw = UInt(3.W)
  val br_comp_ctrl = UInt(SZ_B_CTRL.W)
  val b = Bool()
  val bl = Bool()
  val jirl = Bool()
  val imm26 = UInt(26.W)
}

class Decode extends Module {
  val io = IO(new Bundle() {
    val instr = Input(UInt(32.W))
    val ctrl = Output(new CtrlSignals)
  })

  io.ctrl.rj := io.instr(9, 5)
  io.ctrl.rk := io.instr(14, 10)
  io.ctrl.rd := io.instr(4, 0)
  io.ctrl.imm26 := io.instr(25, 0)

  io.ctrl.alu_ctrl := DontCare
  when(io.instr === ADD_W || io.instr === ADDI_W ||
    io.instr === LD_W || io.instr === ST_W ||
    io.instr === LD_BU || io.instr === LD_B || io.instr === ST_B ||
    io.instr === LD_HU || io.instr === LD_H || io.instr === ST_H ||
    io.instr === JIRL || io.instr === BL){
    io.ctrl.alu_ctrl := ALU_ADD
  }.elsewhen(io.instr === SUB_W){
    io.ctrl.alu_ctrl := ALU_SUB
  }.elsewhen(io.instr === SLT || io.instr === SLTI){
    io.ctrl.alu_ctrl := ALU_SLT
  }.elsewhen(io.instr === SLTU || io.instr === SLTUI){
    io.ctrl.alu_ctrl := ALU_SLTU
  }.elsewhen(io.instr === SLL_W || io.instr === SLLI_W || io.instr === LU12I_W){
    io.ctrl.alu_ctrl := ALU_SLL
  }.elsewhen(io.instr === SRL_W || io.instr === SRLI_W){
    io.ctrl.alu_ctrl := ALU_SRL
  }.elsewhen(io.instr === SRA_W || io.instr === SRAI_W){
    io.ctrl.alu_ctrl := ALU_SRA
  }.elsewhen(io.instr === AND){
    io.ctrl.alu_ctrl := ALU_AND
  }.elsewhen(io.instr === OR){
    io.ctrl.alu_ctrl := ALU_OR
  }.elsewhen(io.instr === XOR){
    io.ctrl.alu_ctrl := ALU_XOR
  }.elsewhen(io.instr === NOR){
    io.ctrl.alu_ctrl := ALU_NOR
  }

  io.ctrl.muldiv_ctrl := Cat(io.instr === DIV_W || io.instr === MOD_W || io.instr === DIV_WU || io.instr === MOD_WU,
    io.instr === MULH_WU || io.instr === MULH_W || io.instr === DIV_W || io.instr === DIV_WU,
    io.instr === MULH_W || io.instr === MUL_W || io.instr === DIV_W || io.instr === MOD_W)

  io.ctrl.sel_src := Cat(io.instr === LU12I_W,
    io.instr === JIRL || io.instr === BL,
    io.instr === SLLI_W || io.instr === SRLI_W || io.instr === SRAI_W,
    io.instr === ANDI || io.instr === ORI || io.instr === XORI,
    io.instr === ADDI_W || io.instr === SLTI || io.instr === SLTUI ||
      io.instr === LD_BU || io.instr === LD_B || io.instr === ST_B ||
      io.instr === LD_HU || io.instr === LD_H || io.instr === ST_H ||
      io.instr === LD_W || io.instr === ST_W )

  io.ctrl.mem2reg := io.instr === LD_W || io.instr === LD_B || io.instr === LD_H || io.instr === LD_BU || io.instr === LD_HU

  io.ctrl.reg2mem := io.instr === ST_W || io.instr === ST_B || io.instr === ST_H

  io.ctrl.bhw := Cat(io.instr === LD_B || io.instr === LD_H,
    io.instr === LD_W || io.instr === ST_W,
    io.instr === LD_H || io.instr === LD_HU || io.instr === ST_H)

  io.ctrl.br_comp_ctrl := Cat(io.instr === BNE || io.instr === BGE || io.instr === BGEU,
    io.instr === BLT || io.instr === BGE || io.instr === BLTU || io.instr === BGEU,
    io.instr === BEQ || io.instr === BNE || io.instr === BLT || io.instr === BGE)

  io.ctrl.b := io.instr === B

  io.ctrl.bl := io.instr === BL

  io.ctrl.jirl := io.instr === JIRL

}