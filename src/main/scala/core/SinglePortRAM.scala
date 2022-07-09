package core

import chisel3._
import chisel3.util._

class SinglePortRAM extends Module {
  val io = IO(new Bundle() {
    val raddr = Flipped(Valid(UInt(14.W)))
    val rdata = Output(UInt(32.W))
    val waddr = Flipped(Valid(UInt(14.W)))
    val wdata = Input(UInt(32.W))
  })

  val ram = Mem(1 << 14, UInt(32.W))

  io.rdata := Mux(io.raddr.valid, ram(io.raddr.bits), 0.U)

  when(io.waddr.valid) {
    ram(io.waddr.bits) := io.wdata
  }

}
