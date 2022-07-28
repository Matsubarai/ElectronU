package core

import chisel3._
import chisel3.util._

object ALU
{
  val SZ_ALU_CTRL = 4
  def ALU_ADD  = "b0000".U
  def ALU_SUB  = "b0001".U

  def ALU_SLT  = "b1001".U
  def ALU_SLTU = "b1011".U

  def ALU_NOR  = "b0100".U
  def ALU_AND  = "b0101".U
  def ALU_OR   = "b0111".U
  def ALU_XOR  = "b0110".U

  def ALU_SLL  = "b1100".U
  def ALU_SRL  = "b1110".U
  def ALU_SRA  = "b1111".U
}

import ALU._

class ALU extends Module {
  val io = IO(new Bundle() {
    val ctrl = Input(UInt(SZ_ALU_CTRL.W))
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val result = Output(UInt(32.W))
  })

  val is_sub = io.ctrl(0) //sub, slt, sltu, sra
  val src2_inv = Mux(is_sub, ~io.src2, io.src2)
  val adder_result = io.src1 + src2_inv + is_sub

  val is_sltu = io.ctrl(1) //sltu, srl, sra
  val slt_result = Mux(io.src1(31) === io.src2(31), adder_result(31), Mux(is_sltu, io.src2(31), io.src1(31)))

  val shamt = io.src2(4, 0)
  val shift_src = Mux(is_sltu, io.src1, Reverse(io.src1))
  val shift_result_r = (Cat(is_sub & shift_src(31), shift_src).asSInt >> shamt)(31, 0)
  val shift_result_l = Reverse(shift_result_r)
  val shift_result = Mux(is_sltu, shift_result_r, shift_result_l)

  val or_result = io.src1 | io.src2
  val logic_result = Mux1H(UIntToOH(io.ctrl(1, 0)), Seq(~or_result, io.src1 & io.src2, io.src1 ^ io.src2, or_result))

  io.result := Mux1H(UIntToOH(io.ctrl(3, 2)), Seq(adder_result, logic_result, Cat(0.U(31.W), slt_result), shift_result))
}