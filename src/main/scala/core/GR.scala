package core

import chisel3._
import chisel3.util._

class GR extends Module {
  val io = IO(new Bundle() {
    val raddr1 = Flipped(Valid(UInt(5.W)))
    val raddr2 = Flipped(Valid(UInt(5.W)))
    val waddr = Flipped(Valid(UInt(5.W)))
    val rdata1 = Output(UInt(32.W))
    val rdata2 = Output(UInt(32.W))
    val wdata = Input(UInt(32.W))
  })

  val regfile = Mem(32, UInt(32.W))

  io.rdata1 := Mux(io.raddr1.bits === 0.U || !io.raddr1.valid, 0.U, regfile(io.raddr1.bits))

  io.rdata2 := Mux(io.raddr2.bits === 0.U || !io.raddr2.valid, 0.U, regfile(io.raddr2.bits))

  when(io.waddr.valid && io.waddr.bits =/= 0.U){
    regfile(io.waddr.bits) := io.wdata
  }
}