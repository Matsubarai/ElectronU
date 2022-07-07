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
  val sel_wreg = Bool()
  val imm12 = UInt(12.W)
}

class Decode extends Module {
  val io = IO(new Bundle() {
    val instr = Input(UInt(32.W))
    val ctrl = Output(new CtrlSignals)
  })

  io.ctrl.rj := io.instr(9, 5)
  io.ctrl.rk := io.instr(14, 10)
  io.ctrl.rd := io.instr(4, 0)
  io.ctrl.imm12 := io.instr(21, 10)

  when(io.instr === ADD_W || io.instr === ADDI_W || io.instr === LD_W || io.instr === ST_W){
    io.ctrl.alu_ctrl := ALU_ADD
  }.elsewhen(io.ctrl.alu_ctrl === SUB_W){
    io.ctrl.alu_ctrl := ALU_SUB
  }

  when(io.instr === ADDI_W || io.instr === LD_W){
    io.ctrl.sel_src2 := 1.B
  }.otherwise{
    io.ctrl.sel_src2 := 0.B
  }

  when(io.instr === LD_W){
    io.ctrl.sel_wreg := 1.B
  }.otherwise{
    io.ctrl.sel_wreg := 0.B
  }
}