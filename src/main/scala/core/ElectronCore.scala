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
  val if_id_reg_instr = Pipe(1.B, Mux(br_taken_flush, "b0000001010_000000000000_00000_00000".U, iram.io.rdata))

  val decode = Module(new Decode)
  decode.io.instr := if_id_reg_instr.bits

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

  //EXE
  val id_exe_reg_ctrl = Pipe(1.B, decode.io.ctrl)
  val id_exe_reg_rdata1 = Pipe(1.B, rf.io.rdata1)
  val id_exe_reg_rdata2 = Pipe(1.B, rf.io.rdata2)
  val id_exe_reg_rf_wen = Pipe(1.B, !(decode.io.ctrl.reg2mem || decode.io.ctrl.branch))
  val id_exe_reg_rf_waddr = Pipe(1.B, Mux(decode.io.ctrl.bl, 1.U(5.W), decode.io.ctrl.rd))

  val alu = Module(new ALU)
  alu.io.ctrl := id_exe_reg_ctrl.bits.alu_ctrl
  val si12 = Cat(Fill(20, id_exe_reg_ctrl.bits.imm26(21)), id_exe_reg_ctrl.bits.imm26(21,10))
  alu.io.src1 := id_exe_reg_rdata1.bits
  alu.io.src2 := Mux(id_exe_reg_ctrl.bits.sel_src2, si12, id_exe_reg_rdata2.bits)

  //MEM
  val exe_mem_reg_alu_result = Pipe(1.B, alu.io.result)
  val exe_mem_reg_mem2reg = Pipe(1.B, id_exe_reg_ctrl.bits.mem2reg)
  val exe_mem_reg_lui = Pipe(1.B, id_exe_reg_ctrl.bits.lui)
  val exe_mem_reg_imm26 = Pipe(1.B, id_exe_reg_ctrl.bits.imm26)
  val exe_mem_reg_rf_wen = Pipe(id_exe_reg_rf_wen)
  val exe_mem_reg_rf_waddr = Pipe(id_exe_reg_rf_waddr)

  val d_addr_trans = Module(new AddrTrans)
  d_addr_trans.io.vaddr := alu.io.result // pre-MEM

  val dram = Module(new SinglePortSyncRAM)
  dram.io.en := id_exe_reg_ctrl.bits.mem2reg || id_exe_reg_ctrl.bits.reg2mem //pre-MEM
  dram.io.addr := d_addr_trans.io.paddr(15, 2) //pre-MEM
  dram.io.wen := id_exe_reg_ctrl.bits.reg2mem //pre-MEM
  dram.io.wdata := id_exe_reg_rdata2.bits //pre-MEM

  //WB
  val mem_wb_reg_rdata = Pipe(1.B, dram.io.rdata)
  val mem_wb_reg_alu_result = Pipe(exe_mem_reg_alu_result)
  val mem_wb_reg_mem2reg = Pipe(exe_mem_reg_mem2reg)
  val mem_wb_reg_lui = Pipe(exe_mem_reg_lui)
  val mem_wb_reg_imm26 = Pipe(exe_mem_reg_imm26)
  val mem_wb_reg_rf_wen = Pipe(exe_mem_reg_rf_wen)
  val mem_wb_reg_rf_waddr = Pipe(exe_mem_reg_rf_waddr)

  val si20 = Cat(mem_wb_reg_imm26.bits(24,5), 0.U(12.W))
  rf.io.waddr.valid := mem_wb_reg_rf_wen.bits
  rf.io.waddr.bits := mem_wb_reg_rf_waddr.bits
  rf.io.wdata := Mux(mem_wb_reg_lui.bits, si20, Mux(mem_wb_reg_mem2reg.bits, mem_wb_reg_rdata.bits, mem_wb_reg_alu_result.bits))
}

object Generator extends App{
  (new ChiselStage).emitVerilog(new ElectronCore, args)
}