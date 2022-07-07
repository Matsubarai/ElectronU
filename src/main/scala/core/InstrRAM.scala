package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile

class InstrRAM extends Module {
  val io = IO(new Bundle() {
    val raddr = Flipped(Valid(UInt(14.W)))
    val rdata = Output(UInt(32.W))
  })

  val ram = Mem(1<<14, UInt(32.W))

  when(io.raddr.valid){
    io.rdata := ram(io.raddr.bits)
  }.otherwise{
    io.rdata := 0.U
  }
}
