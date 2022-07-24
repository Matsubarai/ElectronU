package core

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._

class ElectronCore extends Module {
  val io = IO(new Bundle() {
    val pc = Valid(UInt(32.W))
    val imem = Flipped(new MemPort)
    val dmem = Flipped(new MemPort)
  })
  printf("----------\n")
  //IF
  val ld_stall = Wire(Bool())
  val fetch = Module(new Fetch)
  fetch.io.stall := ld_stall

  val i_addr_trans = Module(new AddrTrans)
  i_addr_trans.io.vaddr := Mux(ld_stall, fetch.io.pc.bits, fetch.io.npc) //pre-IF
  io.pc <> fetch.io.pc

  printf(p"pre-IF_IF: ${fetch.io.pc.valid}\n")
  printf(p"pc:    0x${Hexadecimal(fetch.io.pc.bits)}\n")

  io.imem.en := 1.B
  io.imem.addr := i_addr_trans.io.paddr(15,2) //pre-IF
  io.imem.wen := 0.B
  io.imem.wdata := DontCare

  val jump_flush = Wire(Bool())
  val if_id_sigs = Wire(new Bundle {
    val valid = Bool()
    val bits = new Bundle() {
      val instr = UInt()
      val pc = UInt()
    }
  })
  if_id_sigs.valid := fetch.io.pc.valid && !jump_flush
  if_id_sigs.bits.instr := io.imem.rdata
  if_id_sigs.bits.pc := fetch.io.pc.bits

  //ID
  val if_id = RegEnable(if_id_sigs, !ld_stall)

  printf(p"IF_ID: ${if_id.valid}\n")
  printf(p"pc:    0x${Hexadecimal(if_id.bits.pc)}\n")
  printf(p"instr: 0x${Hexadecimal(if_id.bits.instr)}\n")

  val decode = Module(new Decode)
  decode.io.instr := if_id.bits.instr

  val rf = Module(new GR)
  rf.io.raddr1.valid := if_id.valid
  rf.io.raddr2.valid := if_id.valid
  rf.io.raddr1.bits := decode.io.ctrl.rj
  rf.io.raddr2.bits := Mux(decode.io.ctrl.reg2mem || decode.io.ctrl.br_comp_ctrl.orR, decode.io.ctrl.rd, decode.io.ctrl.rk)
  val rf_rdata1 = Wire(UInt(32.W))
  val rf_rdata2 = Wire(UInt(32.W))

  val br_comp = Module(new BranchCompare)
  br_comp.io.ctrl := decode.io.ctrl.br_comp_ctrl
  br_comp.io.src1 := rf_rdata1
  br_comp.io.src2 := rf_rdata2
  val br_taken = decode.io.ctrl.br_comp_ctrl.orR && !ld_stall && br_comp.io.result
  val jump = decode.io.ctrl.b || decode.io.ctrl.bl || br_taken || decode.io.ctrl.jirl
  val offs16 = Cat(Fill(14, decode.io.ctrl.imm26(25)), decode.io.ctrl.imm26(25, 10), 0.U(2.W))
  val offs26 = Cat(Fill(4, decode.io.ctrl.imm26(25)), decode.io.ctrl.imm26(25, 0), 0.U(2.W))

  jump_flush := if_id.valid && jump

  fetch.io.offs.valid := jump_flush
  fetch.io.offs.bits := Mux(decode.io.ctrl.bl || decode.io.ctrl.b, offs26, offs16)
  fetch.io.base.valid := jump_flush
  fetch.io.base.bits := Mux(decode.io.ctrl.jirl, rf_rdata1, if_id.bits.pc)

  val id_exe_sigs = Wire(new Bundle{
    val alu_ctrl = UInt()
    val muldiv_ctrl = UInt()
    val sel_src1 = UInt()
    val sel_src2 = UInt()
    val reg2mem = Bool()
    val mem2reg = Bool()
    val imm26 = UInt()
    val rf_rdata1 = UInt()
    val rf_rdata2 = UInt()
    val rf_wen = Bool()
    val rf_waddr = UInt()
    val pc = UInt()
  })
  id_exe_sigs.alu_ctrl := decode.io.ctrl.alu_ctrl
  id_exe_sigs.muldiv_ctrl := decode.io.ctrl.muldiv_ctrl
  id_exe_sigs.sel_src1 := Cat(decode.io.ctrl.sel_src(4, 3), !decode.io.ctrl.sel_src(4, 3).orR)
  id_exe_sigs.sel_src2 := Cat(decode.io.ctrl.sel_src, !decode.io.ctrl.sel_src.orR)
  id_exe_sigs.reg2mem := decode.io.ctrl.reg2mem
  id_exe_sigs.mem2reg := decode.io.ctrl.mem2reg
  id_exe_sigs.imm26 := decode.io.ctrl.imm26
  id_exe_sigs.rf_rdata1 := rf_rdata1
  id_exe_sigs.rf_rdata2 := rf_rdata2
  id_exe_sigs.rf_wen := !(decode.io.ctrl.reg2mem || decode.io.ctrl.br_comp_ctrl.orR || decode.io.ctrl.b)
  id_exe_sigs.rf_waddr := Mux(decode.io.ctrl.bl, 1.U(5.W), decode.io.ctrl.rd)
  id_exe_sigs.pc := if_id.bits.pc

  //EXE
  val id_exe = Pipe(if_id.valid && !ld_stall, id_exe_sigs)

  printf(p"ID_EXE: ${id_exe.valid}\n")

  ld_stall := if_id.valid && id_exe.valid && id_exe.bits.mem2reg && id_exe.bits.rf_waddr =/= 0.U &&
    (id_exe.bits.rf_waddr === rf.io.raddr1.bits || id_exe.bits.rf_waddr === rf.io.raddr2.bits)

  val alu = Module(new ALU)
  alu.io.ctrl := id_exe.bits.alu_ctrl
  val si12 = Cat(Fill(20, id_exe.bits.imm26(21)), id_exe.bits.imm26(21,10))
  val ui12 = id_exe.bits.imm26(21,10)
  val ui5 = id_exe.bits.imm26(14,10)
  val si20 = id_exe.bits.imm26(24, 5)
  alu.io.src1 := Mux1H(id_exe.bits.sel_src1, Seq(id_exe.bits.rf_rdata1, id_exe.bits.pc, si20))
  alu.io.src2 := Mux1H(id_exe.bits.sel_src2, Seq(id_exe.bits.rf_rdata2, si12, ui12, ui5, 4.U(32.W), 12.U(32.W)))

  val muldiv = Module(new MulDiv)
  muldiv.io.ctrl := id_exe.bits.muldiv_ctrl
  muldiv.io.src1 := id_exe.bits.rf_rdata1
  muldiv.io.src2 := id_exe.bits.rf_rdata2

  val alu_result = Mux(id_exe.bits.muldiv_ctrl.orR, muldiv.io.result, alu.io.result)

  val exe_mem_sigs = Wire(new Bundle{
    val alu_result = UInt()
    val mem2reg = Bool()
    val rf_wen = Bool()
    val rf_waddr = UInt()
  })
  exe_mem_sigs.alu_result := alu_result
  exe_mem_sigs.mem2reg := id_exe.bits.mem2reg
  exe_mem_sigs.rf_wen := id_exe.bits.rf_wen
  exe_mem_sigs.rf_waddr := id_exe.bits.rf_waddr

  //MEM
  val exe_mem = Pipe(id_exe.valid, exe_mem_sigs)

  printf(p"EXE_MEM: ${exe_mem.valid}\n")

  val d_addr_trans = Module(new AddrTrans)
  d_addr_trans.io.vaddr := alu_result // pre-MEM

  io.dmem.en := id_exe.valid && (id_exe.bits.mem2reg || id_exe.bits.reg2mem) //pre-MEM
  io.dmem.addr := d_addr_trans.io.paddr(15, 2) //pre-MEM
  io.dmem.wen := id_exe.valid && id_exe.bits.reg2mem //pre-MEM
  io.dmem.wdata := id_exe.bits.rf_rdata2 //pre-MEM

  val rf_wdata = Mux(exe_mem.bits.mem2reg, io.dmem.rdata, exe_mem.bits.alu_result)

  val mem_wb_sigs = Wire(new Bundle{
    val rf_wen = Bool()
    val rf_waddr = UInt()
    val rf_wdata = UInt()
  })
  mem_wb_sigs.rf_wen := exe_mem.bits.rf_wen
  mem_wb_sigs.rf_waddr := exe_mem.bits.rf_waddr
  mem_wb_sigs.rf_wdata := rf_wdata

  //WB
  val mem_wb = Pipe(exe_mem.valid, mem_wb_sigs)

  printf(p"MEM_WB: ${mem_wb.valid}\n")

  rf.io.waddr.valid := mem_wb.valid && mem_wb.bits.rf_wen
  rf.io.waddr.bits := mem_wb.bits.rf_waddr
  rf.io.wdata := mem_wb.bits.rf_wdata

  when(rf.io.waddr.valid) {
    printf("--\n")
    printf(p"wreg:  r${Hexadecimal(rf.io.waddr.bits)}\n")
    printf(p"wdata: 0x${Hexadecimal(rf.io.wdata)}\n")
  }

  //Forwarding
  def isForward(wen: Bool, waddr: UInt, raddr: UInt) = wen && waddr === raddr && waddr =/= 0.U

  val forward_r1_exe = isForward(id_exe.valid && id_exe.bits.rf_wen && !id_exe.bits.mem2reg, id_exe.bits.rf_waddr, rf.io.raddr1.bits)
  val forward_r1_mem = isForward(exe_mem.valid && exe_mem.bits.rf_wen, exe_mem.bits.rf_waddr, rf.io.raddr1.bits)
  val forward_r1_wb = isForward(mem_wb.valid && mem_wb.bits.rf_wen, mem_wb.bits.rf_waddr, rf.io.raddr1.bits)

  rf_rdata1 := PriorityMux(Seq(
    forward_r1_exe -> alu_result,
    forward_r1_mem -> rf_wdata,
    forward_r1_wb -> mem_wb.bits.rf_wdata,
    1.B -> rf.io.rdata1
  ))

  val forward_r2_exe = isForward(id_exe.valid && id_exe.bits.rf_wen && !id_exe.bits.mem2reg, id_exe.bits.rf_waddr, rf.io.raddr2.bits)
  val forward_r2_mem = isForward(exe_mem.valid && exe_mem.bits.rf_wen, exe_mem.bits.rf_waddr, rf.io.raddr2.bits)
  val forward_r2_wb = isForward(mem_wb.valid && mem_wb.bits.rf_wen, mem_wb.bits.rf_waddr, rf.io.raddr2.bits)

  rf_rdata2 := PriorityMux(Seq(
    forward_r2_exe -> alu_result,
    forward_r2_mem -> rf_wdata,
    forward_r2_wb -> mem_wb.bits.rf_wdata,
    1.B -> rf.io.rdata2
  ))
}

object coreGenerator extends App{
  (new ChiselStage).emitVerilog(new ElectronCore, args)
}