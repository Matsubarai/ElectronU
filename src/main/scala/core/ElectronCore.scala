package core

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._

class ElectronCore extends Module {
  val io = IO(new Bundle() {
    val pc = Valid(UInt(32.W))
    val imem = Flipped(new MemPort)
    val dmem = Flipped(new MemPort)
    val ipi = Input(Bool())
    val hwi = Input(UInt(8.W))
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
  io.imem.mask := DontCare
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

  //CSR
  val csr = Module(new CSR)

  val csr_num = decode.io.ctrl.bits.imm26(23, 10)
  csr.io.ctrl.ipi := io.ipi
  csr.io.ctrl.hwi := io.hwi

  val excp_int = csr.io.ctrl.int
  val excp_ine = !decode.io.ctrl.valid
  val excp_brk = decode.io.ctrl.bits.brk
  val excp_sys = decode.io.ctrl.bits.sys

  //GR
  val rf = Module(new GR)
  rf.io.raddr1.valid := if_id.valid
  rf.io.raddr2.valid := if_id.valid
  rf.io.raddr1.bits := decode.io.ctrl.bits.rj
  rf.io.raddr2.bits := Mux(decode.io.ctrl.bits.reg2mem || decode.io.ctrl.bits.br_comp_ctrl.orR ||
    decode.io.ctrl.bits.csr && decode.io.ctrl.bits.rj =/= 0.U, decode.io.ctrl.bits.rd, decode.io.ctrl.bits.rk)
  val rf_rdata1 = Wire(UInt(32.W))
  val rf_rdata2 = Wire(UInt(32.W))

  //JUMP
  val br_comp = Module(new BranchCompare)
  br_comp.io.ctrl := decode.io.ctrl.bits.br_comp_ctrl
  br_comp.io.src1 := rf_rdata1
  br_comp.io.src2 := rf_rdata2
  val br_taken = decode.io.ctrl.bits.br_comp_ctrl.orR && !ld_stall && br_comp.io.result
  val jump = decode.io.ctrl.bits.b || decode.io.ctrl.bits.bl || br_taken || decode.io.ctrl.bits.jirl
  val offs16 = Cat(Fill(14, decode.io.ctrl.bits.imm26(25)), decode.io.ctrl.bits.imm26(25, 10), 0.U(2.W))
  val offs26 = Cat(Fill(4, decode.io.ctrl.bits.imm26(25)), decode.io.ctrl.bits.imm26(25, 0), 0.U(2.W))

  jump_flush := if_id.valid && jump

  fetch.io.offs.valid := jump_flush
  fetch.io.offs.bits := Mux(decode.io.ctrl.bits.bl || decode.io.ctrl.bits.b, offs26, offs16)
  fetch.io.base.valid := jump_flush
  fetch.io.base.bits := Mux(decode.io.ctrl.bits.jirl, rf_rdata1, if_id.bits.pc)

  val id_exe_sigs = Wire(new Bundle{
    val alu_ctrl = UInt()
    val muldiv_ctrl = UInt()
    val sel_src1 = UInt()
    val sel_src2 = UInt()
    val reg2mem = Bool()
    val mem2reg = Bool()
    val bhw = UInt()
    val imm26 = UInt()
    val rf_rdata1 = UInt()
    val rf_rdata2 = UInt()
    val rf_wen = Bool()
    val rf_waddr = UInt()
    val csr_wen = Bool()
    val csr_ren = Bool()
    val csr_num = UInt()
    val pc = UInt()
  })
  id_exe_sigs.alu_ctrl := decode.io.ctrl.bits.alu_ctrl
  id_exe_sigs.muldiv_ctrl := decode.io.ctrl.bits.muldiv_ctrl
  id_exe_sigs.sel_src1 := Cat(decode.io.ctrl.bits.sel_src(5, 3), !decode.io.ctrl.bits.sel_src(5, 3).orR)
  id_exe_sigs.sel_src2 := Cat(decode.io.ctrl.bits.sel_src, !decode.io.ctrl.bits.sel_src.orR)
  id_exe_sigs.reg2mem := decode.io.ctrl.bits.reg2mem
  id_exe_sigs.mem2reg := decode.io.ctrl.bits.mem2reg
  id_exe_sigs.bhw := decode.io.ctrl.bits.bhw
  id_exe_sigs.imm26 := decode.io.ctrl.bits.imm26
  id_exe_sigs.rf_wen := !(decode.io.ctrl.bits.reg2mem || id_exe_sigs.csr_wen ||
    decode.io.ctrl.bits.br_comp_ctrl.orR || decode.io.ctrl.bits.b)
  id_exe_sigs.rf_waddr := Mux(decode.io.ctrl.bits.bl, 1.U(5.W), decode.io.ctrl.bits.rd)
  id_exe_sigs.rf_rdata1 := rf_rdata1
  id_exe_sigs.rf_rdata2 := rf_rdata2
  id_exe_sigs.csr_wen := 0.B//decode.io.ctrl.bits.csr && decode.io.ctrl.bits.rj =/= 0.U
  id_exe_sigs.csr_ren := 0.B//decode.io.ctrl.bits.csr && decode.io.ctrl.bits.rj =/= 1.U
  id_exe_sigs.csr_num := csr_num
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
  val si20 = Cat(id_exe.bits.imm26(24, 5), 0.U(12.W))
  alu.io.src1 := Mux1H(id_exe.bits.sel_src1, Seq(id_exe.bits.rf_rdata1, id_exe.bits.pc, id_exe.bits.pc, 0.U))
  alu.io.src2 := Mux1H(id_exe.bits.sel_src2, Seq(id_exe.bits.rf_rdata2, si12, ui12, ui5, 4.U(32.W), si20, si20))

  val muldiv = Module(new MulDiv)
  muldiv.io.ctrl := id_exe.bits.muldiv_ctrl
  muldiv.io.src1 := id_exe.bits.rf_rdata1
  muldiv.io.src2 := id_exe.bits.rf_rdata2

  val alu_result = Mux(id_exe.bits.muldiv_ctrl.orR, muldiv.io.result, alu.io.result)

  val excp_ale = (id_exe.bits.mem2reg || id_exe.bits.reg2mem) &&
    Mux1H(UIntToOH(id_exe.bits.bhw(1, 0)), Seq(0.B, alu.io.result(0), alu.io.result(1, 0).orR))

  printf(p"alu_src1:    0x${Hexadecimal(alu.io.src1)}\n")
  printf(p"alu_src2:    0x${Hexadecimal(alu.io.src2)}\n")
  printf(p"aluop:       0x${Hexadecimal(alu.io.ctrl)}\n")
  printf(p"alu_result:  0x${Hexadecimal(alu.io.result)}\n")

  val d_addr_trans = Module(new AddrTrans)

  val exe_mem_sigs = Wire(new Bundle{
    val alu_result = UInt()
    val mem2reg = Bool()
    val bhw = UInt()
    val offset = UInt()
    val rf_rdata1 = UInt()
    val rf_rdata2 = UInt()
    val rf_wen = Bool()
    val rf_waddr = UInt()
    val csr_wen = Bool()
    val csr_ren = Bool()
    val csr_num = UInt()
  })
  exe_mem_sigs.alu_result := alu_result
  exe_mem_sigs.mem2reg := id_exe.bits.mem2reg
  exe_mem_sigs.bhw := id_exe.bits.bhw
  exe_mem_sigs.offset := d_addr_trans.io.paddr(1, 0)
  exe_mem_sigs.rf_rdata1 := id_exe.bits.rf_rdata1
  exe_mem_sigs.rf_rdata2 := id_exe.bits.rf_rdata2
  exe_mem_sigs.rf_wen := id_exe.bits.rf_wen
  exe_mem_sigs.rf_waddr := id_exe.bits.rf_waddr
  exe_mem_sigs.csr_wen := id_exe.bits.csr_wen
  exe_mem_sigs.csr_num := id_exe.bits.csr_num
  exe_mem_sigs.csr_ren := id_exe.bits.csr_ren

  //MEM
  val exe_mem = Pipe(id_exe.valid, exe_mem_sigs)

  printf(p"EXE_MEM: ${exe_mem.valid}\n")

  d_addr_trans.io.vaddr := alu.io.result // pre-MEM

  io.dmem.en := id_exe.valid && (id_exe.bits.mem2reg || id_exe.bits.reg2mem) //pre-MEM
  io.dmem.addr := d_addr_trans.io.paddr(15, 2) //pre-MEM
  io.dmem.wen := id_exe.valid && id_exe.bits.reg2mem //pre-MEM
  io.dmem.mask := Mux1H(UIntToOH(id_exe.bits.bhw(1, 0)),
    Seq(UIntToOH(d_addr_trans.io.paddr(1, 0)),
      Mux(d_addr_trans.io.paddr(1), "b1100".U, "b0011".U),
      "b1111".U)) //pre-MEM
  io.dmem.wdata := Mux1H(UIntToOH(id_exe.bits.bhw(1, 0)),
    Seq(Fill(4, id_exe.bits.rf_rdata2(7, 0)),
      Fill(2, id_exe.bits.rf_rdata2(15, 0)),
      id_exe.bits.rf_rdata2)) //pre-MEM

  val is_signed = exe_mem.bits.bhw(2)
  val dmem_rdata_b = Mux1H(UIntToOH(exe_mem.bits.offset), Seq(io.dmem.rdata(7, 0), io.dmem.rdata(15, 8), io.dmem.rdata(23, 16), io.dmem.rdata(31, 24)))
  val dmem_rdata_h = Mux(exe_mem.bits.offset(1), io.dmem.rdata(31, 16), io.dmem.rdata(15, 0))
  val dmem_rdata_w = io.dmem.rdata
  val dmem_rdata = Mux1H(UIntToOH(exe_mem.bits.bhw(1, 0)),
    Seq(Cat(Fill(24, Mux(is_signed, dmem_rdata_b(7), 0.B)), dmem_rdata_b),
      Cat(Fill(16, Mux(is_signed, dmem_rdata_h(15), 0.B)), dmem_rdata_h),
      dmem_rdata_w))
  val rf_wdata_no_csr = Mux(exe_mem.bits.mem2reg, dmem_rdata, exe_mem.bits.alu_result)

  val mem_wb_sigs = Wire(new Bundle{
    val rf_rdata1 = UInt()
    val rf_rdata2 = UInt()
    val rf_wen = Bool()
    val rf_waddr = UInt()
    val rf_wdata_no_csr = UInt()
    val csr_wen = Bool()
    val csr_ren = Bool()
    val csr_num = UInt()
  })
  mem_wb_sigs.rf_rdata1 := exe_mem.bits.rf_rdata1
  mem_wb_sigs.rf_rdata2 := exe_mem.bits.rf_rdata2
  mem_wb_sigs.rf_wen := exe_mem.bits.rf_wen
  mem_wb_sigs.rf_waddr := exe_mem.bits.rf_waddr
  mem_wb_sigs.rf_wdata_no_csr := rf_wdata_no_csr
  mem_wb_sigs.csr_wen := exe_mem.bits.csr_wen
  mem_wb_sigs.csr_ren := exe_mem.bits.csr_ren
  mem_wb_sigs.csr_num := exe_mem.bits.csr_num

  //WB
  val mem_wb = Pipe(exe_mem.valid, mem_wb_sigs)

  printf(p"MEM_WB: ${mem_wb.valid}\n")

  val rf_wdata = Mux(mem_wb.bits.csr_ren, csr.io.rdata, mem_wb.bits.rf_wdata_no_csr)
  rf.io.waddr.valid := mem_wb.valid && mem_wb.bits.rf_wen
  rf.io.waddr.bits := mem_wb.bits.rf_waddr
  rf.io.wdata := rf_wdata

  when(rf.io.waddr.valid) {
    printf("--\n")
    printf(p"wreg:  r${Hexadecimal(rf.io.waddr.bits)}\n")
    printf(p"wdata: 0x${Hexadecimal(rf.io.wdata)}\n")
  }

  csr.io.rd_csr_num.valid := mem_wb.valid && mem_wb.bits.csr_ren
  csr.io.rd_csr_num.bits := mem_wb.bits.csr_num
  csr.io.wr_csr_num.valid := mem_wb.valid && mem_wb.bits.csr_wen
  csr.io.wr_csr_num.bits := mem_wb.bits.csr_num
  csr.io.wdata := Mux(mem_wb.bits.csr_ren, csr.io.rdata & (~mem_wb.bits.rf_rdata1).asUInt |
    mem_wb.bits.rf_rdata2 & mem_wb.bits.rf_rdata1, mem_wb.bits.rf_rdata2)

  //Forwarding
  def isForward(wen: Bool, waddr: UInt, raddr: UInt) = wen && waddr === raddr && waddr =/= 0.U

  val forward_r1_exe = isForward(id_exe.valid && id_exe.bits.rf_wen && !id_exe.bits.csr_ren && !id_exe.bits.mem2reg, id_exe.bits.rf_waddr, rf.io.raddr1.bits)
  val forward_r1_mem = isForward(exe_mem.valid && exe_mem.bits.rf_wen && !exe_mem.bits.csr_ren, exe_mem.bits.rf_waddr, rf.io.raddr1.bits)
  val forward_r1_wb = isForward(mem_wb.valid && mem_wb.bits.rf_wen, mem_wb.bits.rf_waddr, rf.io.raddr1.bits)

  rf_rdata1 := PriorityMux(Seq(
    forward_r1_exe -> alu_result,
    forward_r1_mem -> rf_wdata_no_csr,
    forward_r1_wb -> rf_wdata,
    1.B -> rf.io.rdata1
  ))

  val forward_r2_exe = isForward(id_exe.valid && id_exe.bits.rf_wen && !id_exe.bits.csr_ren && !id_exe.bits.mem2reg, id_exe.bits.rf_waddr, rf.io.raddr2.bits)
  val forward_r2_mem = isForward(exe_mem.valid && exe_mem.bits.rf_wen && !exe_mem.bits.csr_ren, exe_mem.bits.rf_waddr, rf.io.raddr2.bits)
  val forward_r2_wb = isForward(mem_wb.valid && mem_wb.bits.rf_wen, mem_wb.bits.rf_waddr, rf.io.raddr2.bits)

  rf_rdata2 := PriorityMux(Seq(
    forward_r2_exe -> alu_result,
    forward_r2_mem -> rf_wdata_no_csr,
    forward_r2_wb -> rf_wdata,
    1.B -> rf.io.rdata2
  ))
}

object coreGenerator extends App{
  (new ChiselStage).emitVerilog(new ElectronCore, args)
}