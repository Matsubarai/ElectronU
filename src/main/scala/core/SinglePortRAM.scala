package core

import chisel3._
import chisel3.util._

class MemPort extends Bundle {
  val en = Input(Bool())
  val wen = Input(Bool())
  val mask = Input(UInt(4.W))
  val addr = Input(UInt(14.W))
  val rdata = Output(UInt(32.W))
  val wdata = Input(UInt(32.W))
}

class SinglePortRAM extends Module {
  val io = IO(new MemPort)

  val ram = Mem(1 << 14, UInt(32.W))

  io.rdata := Mux(io.en, ram(io.addr), DontCare)

  when(io.en && io.wen) {
    ram(io.addr) := io.wdata
  }
}

class SinglePortSyncRAM extends Module {
  val io = IO(new MemPort)

  val wdata = Wire(Vec(4, UInt(8.W)))
  val rdata = Wire(Vec(4, UInt(8.W)))
  val mask = Wire(Vec(4, Bool()))
  wdata := Seq(io.wdata(31, 24), io.wdata(23, 16), io.wdata(15, 8), io.wdata(7, 0))
  mask := Seq(io.mask(3), io.mask(2), io.mask(1), io.mask(0))
  io.rdata := Cat(rdata(3), rdata(2), rdata(1), rdata(0))

  val ram = SyncReadMem(1 << 14, Vec(4, UInt(8.W)))

  io.rdata := DontCare
  when(io.en){
    when(io.wen){
      ram.write(io.addr, wdata, mask)
    }.otherwise{
      rdata := ram.read(io.addr)
    }
  }
}