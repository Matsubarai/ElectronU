package core

import chisel3._
import chisel3.stage.ChiselStage

class ElectronTile extends Module{
  val io = IO(new Bundle() {
    val pc = Output(UInt(32.W))
  })

  val imem = Module(new SinglePortSyncRAM)
  val dmem = Module(new SinglePortSyncRAM)
  val core = Module(new ElectronCore)

  core.io.dmem <> dmem.io
  core.io.imem <> imem.io
  io.pc := core.io.pc
}

object tileGenerator extends App{
  (new ChiselStage).emitVerilog(new ElectronTile, args)
}