package core

import chisel3._
import chisel3.util._
import Instructions._
import ALU._

class CtrlSignals extends Bundle{
  val alu_ctrl = UInt(SZ_ALU_CTRL.W)
  val rj = UInt(5.W)
  val rk = UInt(5.W)
  val rd = UInt(5.W)
  val sel_src2 = UInt(3.W)
  val mem2reg = Bool()
  val reg2mem = Bool()
  val branch = Bool()
  val bne = Bool()
  val bl = Bool()
  val jirl = Bool()
  val lui = Bool()
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
  when(io.instr === ADD_W || io.instr === ADDI_W || io.instr === LD_W || io.instr === ST_W){
    io.ctrl.alu_ctrl := ALU_ADD
  }.elsewhen(io.instr === SUB_W){
    io.ctrl.alu_ctrl := ALU_SUB
  }.elsewhen(io.instr === SLT || io.instr === SLTI){
    io.ctrl.alu_ctrl := ALU_SLT
  }.elsewhen(io.instr === SLTU || io.instr === SLTUI){
    io.ctrl.alu_ctrl := ALU_SLTU
  }.elsewhen(io.instr === SLL_W || io.instr === SLLI_W){
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

  io.ctrl.sel_src2 := Cat(io.instr === SLLI_W || io.instr === SRLI_W || io.instr === SRAI_W,
    io.instr === ANDI || io.instr === ORI || io.instr === XORI,
    io.instr === ADDI_W || io.instr === LD_W || io.instr === ST_W || io.instr === SLTI || io.instr === SLTUI)

  io.ctrl.mem2reg := io.instr === LD_W

  io.ctrl.reg2mem := io.instr === ST_W

  io.ctrl.branch := io.instr === BEQ || io.instr === BNE

  io.ctrl.bne := io.instr === BNE

  io.ctrl.bl := io.instr === BL

  io.ctrl.jirl := io.instr === JIRL

  io.ctrl.lui := io.instr === LU12I_W

}