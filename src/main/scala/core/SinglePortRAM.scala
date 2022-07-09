package core

import chisel3._
import chisel3.util._

//class SinglePortRAM extends Module {
//  val io = IO(new Bundle() {
//    val raddr = Flipped(Valid(UInt(14.W)))
//    val rdata = Output(UInt(32.W))
//    val waddr = Flipped(Valid(UInt(14.W)))
//    val wdata = Input(UInt(32.W))
//  })
//
//  val ram = Mem(1 << 14, UInt(32.W))
//
//  io.rdata := Mux(io.raddr.valid, ram(io.raddr.bits), DontCare)
//
//  when(io.waddr.valid) {
//    ram(io.waddr.bits) := io.wdata
//  }
//
//}

class MemPort extends Bundle {
  val en = Input(Bool())
  val wen = Input(Bool())
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

  val ram = SyncReadMem(1<<14, UInt(32.W))

  io.rdata := DontCare
  when(io.en){
    when(io.wen){
      ram.write(io.addr, io.wdata)
    }.otherwise{
      io.rdata := ram.read(io.addr)
    }
  }
}