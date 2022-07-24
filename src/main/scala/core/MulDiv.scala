package core

import chisel3._
import chisel3.util._

object MulDiv{
  val SZ_MULDIV_CTRL = 3
  def MULDIV_NULL   = "b000".U
  def MULDIV_MUL    = "b001".U
  def MULDIV_MULH   = "b011".U
  def MULDIV_MULHU  = "b010".U
  def MULDIV_DIV    = "b111".U
  def MULDIV_DIVU   = "b110".U
  def MULDIV_MOD    = "b101".U
  def MULDIV_MODU   = "b100".U
}

import MulDiv._

class MulDiv extends Module {
  val io = IO(new Bundle() {
    val ctrl = Input(UInt(SZ_MULDIV_CTRL.W))
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val result = Output(UInt(32.W))
  })
  val is_sign = io.ctrl(0)
  val src1 = Cat(Mux(is_sign, io.src1(31), 0.B), io.src1).asSInt
  val src2 = Cat(Mux(is_sign, io.src2(31), 0.B), io.src2).asSInt

  val mul_result = src1 * src2

  io.result := Mux1H(UIntToOH(io.ctrl(2, 1), 4), Seq(mul_result(31, 0), mul_result(63, 32), (src1 / src2).asUInt, (src1 % src2).asUInt))
}
