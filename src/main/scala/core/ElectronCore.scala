package core

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._

class ElectronCore extends Module {
  val io = IO(new Bundle() {
    val pc = Output(UInt(32.W))
  })

  //IF
  val fetch = Module(new Fetch)

  val i_addr_trans = Module(new AddrTrans)
  i_addr_trans.io.vaddr := fetch.io.pc
  io.pc := fetch.io.pc

  val iram = Module(new SinglePortRAM)
  iram.io.en := 1.B
  iram.io.addr := i_addr_trans.io.paddr(15,2)
  iram.io.wen := 0.B

  //ID
  val decode = Module(new Decode)
  decode.io.instr := iram.io.rdata

  val rf = Module(new GR)
  rf.io.raddr1.valid := 1.B
  rf.io.raddr2.valid := 1.B
  rf.io.waddr.valid := !(decode.io.ctrl.reg2mem || decode.io.ctrl.branch)
  rf.io.raddr1.bits := decode.io.ctrl.rj
  rf.io.raddr2.bits := Mux(decode.io.ctrl.reg2mem || decode.io.ctrl.branch, decode.io.ctrl.rd, decode.io.ctrl.rk)
  rf.io.waddr.bits := Mux(decode.io.ctrl.bl, 1.U(5.W), decode.io.ctrl.rd)

  val br_taken = decode.io.ctrl.branch && ((rf.io.rdata1 === rf.io.rdata2) ^ decode.io.ctrl.bne)
  val offs16 = Cat(Fill(14, decode.io.ctrl.imm26(25)), decode.io.ctrl.imm26(25, 10), 0.U(2.W))
  val offs26 = Cat(Fill(4, decode.io.ctrl.imm26(25)), decode.io.ctrl.imm26(25, 0), 0.U(2.W))

  fetch.io.offs.valid := decode.io.ctrl.bl || br_taken || decode.io.ctrl.jirl
  fetch.io.offs.bits := Mux(decode.io.ctrl.bl, offs26, offs16)
  fetch.io.base.valid := decode.io.ctrl.jirl
  fetch.io.base.bits := rf.io.rdata1

  //EXE
  val alu = Module(new ALU)
  alu.io.ctrl := decode.io.ctrl.alu_ctrl
  val si12 = Cat(Fill(20, decode.io.ctrl.imm26(21)), decode.io.ctrl.imm26(21,10))
  alu.io.src1 := rf.io.rdata1
  alu.io.src2 := Mux(decode.io.ctrl.sel_src2, si12, rf.io.rdata2)

  //MEM
  val d_addr_trans = Module(new AddrTrans)
  d_addr_trans.io.vaddr := alu.io.result

  val dram = Module(new SinglePortRAM)
  dram.io.en := decode.io.ctrl.mem2reg || decode.io.ctrl.reg2mem
  dram.io.addr := d_addr_trans.io.paddr
  dram.io.wen := decode.io.ctrl.reg2mem
  dram.io.wdata := rf.io.rdata2

  //WB
  val si20 = Cat(decode.io.ctrl.imm26(24,5), 0.U(12.W))
  rf.io.wdata := Mux(decode.io.ctrl.lui, si20, Mux(decode.io.ctrl.mem2reg, dram.io.wdata, alu.io.result))
}

object Generator extends App{
  (new ChiselStage).emitVerilog(new ElectronCore, args)
}