package core

import chisel3._
import chisel3.util._

object ALU
{
  val SZ_ALU_CTRL = 4
  def ALU_ADD  = "b0000".U
  def ALU_SLL  = "b0001".U
  def ALU_SEQ  = "b0010".U
  def ALU_SNE  = "b0011".U
  def ALU_XOR  = "b0100".U
  def ALU_SRL  = "b0101".U
  def ALU_OR   = "b0110".U
  def ALU_AND  = "b0111".U
  def ALU_NOR  = "b1001".U
  def ALU_SUB  = "b1010".U
  def ALU_SRA  = "b1011".U
  def ALU_SLT  = "b1100".U
  def ALU_SGE  = "b1101".U
  def ALU_SLTU = "b1110".U
  def ALU_SGEU = "b1111".U
}

import ALU._

class ALU extends Module {
  val io = IO(new Bundle() {
    val ctrl = Input(UInt(SZ_ALU_CTRL.W))
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val result = Output(UInt(32.W))
  })

  val is_sub = io.ctrl(3) //sub, slt, sltu, sra, nor
  val src2_inv = Mux(is_sub, ~io.src2, io.src2)
  val adder_result = io.src1 + src2_inv + is_sub

  val is_sltu = io.ctrl(1)
  val slt_result = Mux(io.src1(31) === io.src2(31), adder_result(31), Mux(is_sltu, io.src2(31), io.src1(31)))

  val shamt = io.src2(4, 0)
  val shift_src = Mux(io.ctrl === ALU_SRL || io.ctrl === ALU_SRA, io.src1, Reverse(io.src1))
  val shift_result_r = ((Cat(is_sub & shift_src(31), shift_src)).asSInt >> shamt)(31, 0)
  val shift_result_l = Reverse(shift_result_r)
  val shift_result = Mux(io.ctrl === ALU_SRL || io.ctrl === ALU_SRA, shift_result_r, shift_result_l)

  val logic_result = Mux(io.ctrl === ALU_XOR || io.ctrl === ALU_OR, io.src1 ^ io.src2, 0.U) |
    Mux(io.ctrl === ALU_OR || io.ctrl === ALU_AND, io.src1 & io.src2, (~(io.src1 | io.src2)).asUInt)

  when(io.ctrl === ALU_ADD || io.ctrl === ALU_SUB){
    io.result := adder_result
  }.elsewhen(io.ctrl === ALU_SLT || io.ctrl === ALU_SLTU){
    io.result := Cat(0.U(31.W), slt_result)
  }.elsewhen(io.ctrl === ALU_SLL || io.ctrl === ALU_SRL || io.ctrl === ALU_SRA){
    io.result := shift_result
  }.elsewhen(io.ctrl === ALU_AND || io.ctrl === ALU_OR || io.ctrl === ALU_XOR || io.ctrl === ALU_NOR){
    io.result := logic_result
  }.otherwise{
    io.result := 0.U
  }
}