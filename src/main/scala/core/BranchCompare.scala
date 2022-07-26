package core

import chisel3._
import chisel3.util._

object BranchCompare{
  val SZ_B_CTRL = 3
  def B_NULL = "b000".U
  def B_BEQ  = "b001".U
  def B_BNE  = "b101".U
  def B_BLT  = "b011".U
  def B_BGE  = "b111".U
  def B_BLTU = "b010".U
  def B_BGEU = "b110".U
}

import BranchCompare._

class BranchCompare extends Module {
  val io = IO(new Bundle() {
    val ctrl = Input(UInt(SZ_B_CTRL.W))
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val result = Output(Bool())
  })
  val is_sign = io.ctrl(0)
  val is_less = io.ctrl(1)
  val is_rev = io.ctrl(2)
  val src1 = Cat(Mux(is_sign, io.src1(31), 0.B), io.src1).asSInt
  val src2 = Cat(Mux(is_sign, io.src2(31), 0.B), io.src2).asSInt

  io.result := Mux(is_less, src1 < src2, src1 === src2) ^ is_rev
}
