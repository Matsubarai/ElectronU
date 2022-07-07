package core

import chisel3._

class AddrTrans extends Module {
  val io = IO(new Bundle() {
    val vaddr = Input(UInt(32.W))
    val paddr = Output(UInt(16.W))
  })

  io.paddr := io.vaddr(15, 0)

}
