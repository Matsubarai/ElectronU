package core

import chisel3._
import Instructions._
import ALU._

class CtrlSignals extends Bundle{
  val alu_ctrl = UInt(SZ_ALU_CTRL.W)
  val rj = UInt(5.W)
  val rk = UInt(5.W)
  val rd = UInt(5.W)
  val sel_src2 = Bool()
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

  when(io.instr === ADD_W || io.instr === ADDI_W || io.instr === LD_W || io.instr === ST_W){
    io.ctrl.alu_ctrl := ALU_ADD
  }.elsewhen(io.instr === SUB_W){
    io.ctrl.alu_ctrl := ALU_SUB
  }.elsewhen(io.instr === SLT){
    io.ctrl.alu_ctrl := ALU_SLT
  }.elsewhen(io.instr === SLTU){
    io.ctrl.alu_ctrl := ALU_SLTU
  }.elsewhen(io.instr === SLL_W){
    io.ctrl.alu_ctrl := ALU_SLL
  }.elsewhen(io.instr === SRL_W){
    io.ctrl.alu_ctrl := ALU_SRL
  }.elsewhen(io.instr === SRA_W){
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

  when(io.instr === ADDI_W || io.instr === LD_W || io.instr === ST_W){
    io.ctrl.sel_src2 := 1.B
  }.otherwise{
    io.ctrl.sel_src2 := 0.B
  }

  when(io.instr === LD_W){
    io.ctrl.mem2reg := 1.B
  }.otherwise{
    io.ctrl.mem2reg := 0.B
  }

  when(io.instr === ST_W){
    io.ctrl.reg2mem := 1.B
  }.otherwise{
    io.ctrl.reg2mem := 0.B
  }

  when(io.instr === BEQ || io.instr === BNE){
    io.ctrl.branch := 1.B
  }.otherwise{
    io.ctrl.branch := 0.B
  }

  when(io.instr === BNE){
    io.ctrl.bne := 1.B
  }.otherwise{
    io.ctrl.bne := 0.B
  }

  when(io.instr === BL){
    io.ctrl.bl := 1.B
  }.otherwise{
    io.ctrl.bl := 0.B
  }

  when(io.instr === JIRL){
    io.ctrl.jirl := 1.B
  }.otherwise{
    io.ctrl.jirl := 0.B
  }

  when(io.instr === LU12I_W){
    io.ctrl.lui := 1.B
  }.otherwise{
    io.ctrl.lui := 0.B
  }
}