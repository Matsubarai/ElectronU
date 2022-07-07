package core

import chisel3._
import chisel3.util._

class Fetch extends Module{
  val io = IO(new Bundle() {
    val pc = Output(UInt(32.W))
    val offs = Flipped(Valid(UInt(32.W)))
    val tgt = Flipped(Valid(UInt(32.W)))
  })

  val pc = RegInit(0x1c000000.U(32.W))

  val npc = Mux(io.tgt.valid, io.tgt.bits, pc + Mux(io.offs.valid, io.offs.bits, 4))

  pc := npc
}