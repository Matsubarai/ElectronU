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
  rf.io.waddr.valid := !(decode.io.ctrl.reg2mem || decode.io.ctrl.branch)
  rf.io.raddr1.bits := decode.io.ctrl.rj
  rf.io.raddr2.bits := Mux(decode.io.ctrl.reg2mem || decode.io.ctrl.branch, decode.io.ctrl.rd, decode.io.ctrl.rk)
  rf.io.waddr.bits := Mux(decode.io.ctrl.bl, 1.U(5.W), decode.io.ctrl.rd)

  val si12 = Cat(Fill(20, decode.io.ctrl.imm26(21)), decode.io.ctrl.imm26(21,10))
  alu.io.src1 := rf.io.rdata1
  alu.io.src2 := Mux(decode.io.ctrl.sel_src2, si12, rf.io.rdata2)

  val br_taken = decode.io.ctrl.branch && ((rf.io.rdata1 === rf.io.rdata2) ^ decode.io.ctrl.bne)
  val offs16 = Cat(Fill(14, decode.io.ctrl.imm26(25)), decode.io.ctrl.imm26(25, 10), 0.U(2.W))
  val offs26 = Cat(Fill(4, decode.io.ctrl.imm26(25)), decode.io.ctrl.imm26(25, 0), 0.U(2.W))
  val tgt = rf.io.rdata1 + offs16

  fetch.io.offs.valid := decode.io.ctrl.bl || br_taken
  fetch.io.offs := Mux(decode.io.ctrl.bl, offs26, offs16)
  fetch.io.tgt.valid := decode.io.ctrl.jirl
  fetch.io.tgt.bits := tgt

  val d_addr_trans = Module(new AddrTrans)
  d_addr_trans.io.vaddr := alu.io.result

  val dram = Module(new DataRAM)
  dram.io.raddr.valid := decode.io.ctrl.mem2reg
  dram.io.raddr.bits := d_addr_trans.io.paddr
  dram.io.waddr.valid := decode.io.ctrl.reg2mem
  dram.io.waddr.bits := d_addr_trans.io.paddr
  dram.io.wdata := rf.io.rdata2

  val si20 = Cat(decode.io.ctrl.imm26(24,5), 0.U(12.W))
  rf.io.wdata := Mux(decode.io.ctrl.lui, si20, Mux(decode.io.ctrl.mem2reg, dram.io.wdata, alu.io.result))
}