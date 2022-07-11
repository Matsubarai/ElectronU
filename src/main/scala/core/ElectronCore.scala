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
  i_addr_trans.io.vaddr := fetch.io.npc //pre-IF
  io.pc := fetch.io.pc

  val iram = Module(new SinglePortRAM)
  iram.io.en := 1.B
  iram.io.addr := i_addr_trans.io.paddr(15,2) //pre-IF
  iram.io.wen := 0.B
  iram.io.wdata := DontCare

  //ID
  val br_taken_flush = Wire(Bool())
  val if_id_instr = Pipe(1.B, Mux(br_taken_flush, "b0000001010_000000000000_00000_00000".U, iram.io.rdata))

  val decode = Module(new Decode)
  decode.io.instr := if_id_instr.bits

  val rf = Module(new GR)
  rf.io.raddr1.valid := 1.B
  rf.io.raddr2.valid := 1.B
  rf.io.raddr1.bits := decode.io.ctrl.rj
  rf.io.raddr2.bits := Mux(decode.io.ctrl.reg2mem || decode.io.ctrl.branch, decode.io.ctrl.rd, decode.io.ctrl.rk)

  val br_taken = decode.io.ctrl.branch && ((rf.io.rdata1 === rf.io.rdata2) ^ decode.io.ctrl.bne)
  val offs16 = Cat(Fill(14, decode.io.ctrl.imm26(25)), decode.io.ctrl.imm26(25, 10), 0.U(2.W))
  val offs26 = Cat(Fill(4, decode.io.ctrl.imm26(25)), decode.io.ctrl.imm26(25, 0), 0.U(2.W))

  fetch.io.offs.valid := decode.io.ctrl.bl || br_taken || decode.io.ctrl.jirl
  fetch.io.offs.bits := Mux(decode.io.ctrl.bl, offs26, offs16)
  fetch.io.base.valid := decode.io.ctrl.jirl
  fetch.io.base.bits := rf.io.rdata1

  br_taken_flush := decode.io.ctrl.bl || br_taken || decode.io.ctrl.jirl

  val id_exe_sigs = new Bundle{
    val alu_ctrl = decode.io.ctrl.alu_ctrl
    val sel_src2 = decode.io.ctrl.sel_src2
    val reg2mem = decode.io.ctrl.reg2mem
    val mem2reg = decode.io.ctrl.mem2reg
    val lui = decode.io.ctrl.lui
    val imm26 = decode.io.ctrl.imm26
    val rf_rdata1 = rf.io.rdata1
    val rf_rdata2 = rf.io.rdata2
    val rf_wen = !(decode.io.ctrl.reg2mem || decode.io.ctrl.branch)
    val rf_waddr = Mux(decode.io.ctrl.bl, 1.U(5.W), decode.io.ctrl.rd)
  }

  //EXE
  val id_exe = Pipe(if_id_instr.valid, id_exe_sigs)

  val alu = Module(new ALU)
  alu.io.ctrl := id_exe.bits.alu_ctrl
  val si12 = Cat(Fill(20, id_exe.bits.imm26(21)), id_exe.bits.imm26(21,10))
  alu.io.src1 := id_exe.bits.rf_rdata1
  alu.io.src2 := Mux(id_exe.bits.sel_src2, si12, id_exe.bits.rf_rdata2)

  val exe_mem_sigs = new Bundle{
    val alu_result = alu.io.result
    val mem2reg = id_exe.bits.mem2reg
    val lui = id_exe.bits.lui
    val imm26 = id_exe.bits.imm26
    val rf_wen = id_exe.bits.rf_wen
    val rf_waddr = id_exe.bits.rf_waddr
  }

  //MEM
  val exe_mem = Pipe(id_exe.valid, exe_mem_sigs)

  val d_addr_trans = Module(new AddrTrans)
  d_addr_trans.io.vaddr := alu.io.result // pre-MEM

  val dram = Module(new SinglePortSyncRAM)
  dram.io.en := id_exe.bits.mem2reg || id_exe.bits.reg2mem //pre-MEM
  dram.io.addr := d_addr_trans.io.paddr(15, 2) //pre-MEM
  dram.io.wen := id_exe.bits.reg2mem //pre-MEM
  dram.io.wdata := id_exe.bits.rf_rdata2 //pre-MEM

  val mem_wb_sigs = new Bundle{
    val dram_rdata = dram.io.rdata
    val alu_result = exe_mem.bits.alu_result
    val mem2reg = exe_mem.bits.mem2reg
    val lui = exe_mem.bits.lui
    val imm26 = exe_mem.bits.imm26
    val rf_wen = exe_mem.bits.rf_wen
    val rf_waddr = exe_mem.bits.rf_waddr
  }

  //WB
  val mem_wb = Pipe(exe_mem.valid, mem_wb_sigs)

  val si20 = Cat(mem_wb.bits.imm26(24, 5), 0.U(12.W))
  rf.io.waddr.valid := mem_wb.bits.rf_wen
  rf.io.waddr.bits := mem_wb.bits.rf_waddr
  rf.io.wdata := Mux(mem_wb.bits.lui, si20, Mux(mem_wb.bits.mem2reg, mem_wb.bits.dram_rdata, mem_wb.bits.alu_result))
}

object Generator extends App{
  (new ChiselStage).emitVerilog(new ElectronCore, args)
}