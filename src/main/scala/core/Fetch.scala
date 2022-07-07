package core

import chisel3._

class Fetch extends Module{
  val io = IO(new Bundle() {
    val pc = Output(UInt(32.W))
  })

  val pc = RegInit(0x1c000000.U(32.W))

  val npc = pc + 4

  pc := npc
}