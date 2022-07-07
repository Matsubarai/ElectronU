package core

import chisel3._

object ALU
{
  val SZ_ALU_CTRL = 4
  def ALU_ADD  = 0.U
  def ALU_SL   = 1.U
  def ALU_SEQ  = 2.U
  def ALU_SNE  = 3.U
  def ALU_XOR  = 4.U
  def ALU_SR   = 5.U
  def ALU_OR   = 6.U
  def ALU_AND  = 7.U
  def ALU_SUB  = 10.U
  def ALU_SRA  = 11.U
  def ALU_SLT  = 12.U
  def ALU_SGE  = 13.U
  def ALU_SLTU = 14.U
  def ALU_SGEU = 15.U
}

import ALU._

class ALU extends Module {
  val io = IO(new Bundle() {
    val ctrl = Input(UInt(SZ_ALU_CTRL.W))
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val result = Output(UInt(32.W))
  })

  val is_sub = io.ctrl(3)
  val src2_inv = Mux(is_sub, ~io.src2, io.src2)
  val adder_result = io.src1 + src2_inv + is_sub

  when(io.ctrl === ALU_ADD || io.ctrl === ALU_SUB){
    io.result := adder_result
  }.otherwise{
    io.result := 0.U
  }
}