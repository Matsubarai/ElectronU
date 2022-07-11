package core

import chisel3._
import chisel3.util._

class Fetch extends Module{
  val io = IO(new Bundle() {
    val pc = Output(UInt(32.W))
    val npc = Output(UInt(32.W))
    val offs = Flipped(Valid(UInt(32.W)))
    val base = Flipped(Valid(UInt(32.W)))
    val stall = Input(Bool())
  })

  val npc = Wire(UInt(32.W))

  val pc = RegEnable(npc, 0x1bffffff.U(32.W), !io.stall)

  npc := Mux(io.base.valid, io.base.bits, pc) + Mux(io.offs.valid, io.offs.bits, 4.U)

  io.pc := pc

  io.npc := npc
}