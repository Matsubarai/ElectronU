package core

import chisel3._
import chisel3.util._

class ElectronCore extends Module {
  val fetch = Module(new Fetch)

  val i_addr_trans = Module(new AddrTrans)
  i_addr_trans.io.vaddr := fetch.io.pc

  val iram = Module(new InstrRAM)
  iram.io.raddr.valid := 1.B
  iram.io.raddr.bits := i_addr_trans.io.paddr(15,2)

  val decode = Module(new Decode)
  decode.io.instr := iram.io.rdata

  val alu = Module(new ALU)
  alu.io.ctrl := decode.io.ctrl.alu_ctrl

  val rf = Module(new GR)
  rf.io.raddr1.valid := 1.B
  rf.io.raddr2.valid := 1.B
  rf.io.waddr.valid := 1.B
  rf.io.raddr1.bits := decode.io.ctrl.rj
  rf.io.raddr2.bits := decode.io.ctrl.rk
  rf.io.waddr.bits := decode.io.ctrl.rd

  val si12 = Cat(Fill(20,decode.io.ctrl.imm12(11)), decode.io.ctrl.imm12)
  alu.io.src1 := rf.io.rdata1
  alu.io.src2 := Mux(decode.io.ctrl.sel_src2, si12, rf.io.rdata2)


  val d_addr_trans = Module(new AddrTrans)
  d_addr_trans.io.vaddr := alu.io.result

  val dram = Module(new DataRAM)
  dram.io.raddr.valid := 1.B
  dram.io.raddr.bits := d_addr_trans.io.paddr
//  dram.io.waddr.valid :=
//  dram.io.waddr.bits := d_addr_trans.io.paddr

  rf.io.wdata := Mux(decode.io.ctrl.sel_wreg, dram.io.wdata, alu.io.result)
}