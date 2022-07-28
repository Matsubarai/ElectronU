package core

import chisel3._
import chisel3.util._


class CSRCtrlSignals extends Bundle{
  val int = Output(Bool())
  val hwi = Input(UInt(8.W))
  val ipi = Input(Bool())
}

object CSR{
  val SZ_CSR_NUM = 14
  def CRMD = 0x0.U
  def PRMD = 0x1.U
  def EUEN = 0x2.U
  def ECFG = 0x4.U
  def ESTAT = 0x5.U
  def ERA = 0x6.U
  def BADV = 0x7.U
  def EENTRY = 0x8.U
  def CPUID = 0x20.U
  def SAVE0 = 0x30.U
  def SAVE1 = 0x31.U
  def SAVE2 = 0x32.U
  def SAVE3 = 0x33.U
  def TID = 0x40.U
  def TCFG = 0x41.U
  def TVAL = 0x42.U
  def TICLR = 0x44.U
  def LLBCTL = 0x60.U
}

object Ecode{
  val SZ_ECODE = 15
  def INT   = "b0000000".U
//  def ADEF  = "b0001000".U
//  def ADEM  = "b1001000".U
  def ALE   = "b0001001".U
  def SYS   = "b0001011".U
  def BRK   = "b0001100".U
  def INE   = "b0001101".U
//  def IPE   = "b0001110".U
//  def FPD   = "b0001111".U
}

import CSR._

class CSR extends Module {
  val io = IO(new Bundle() {
    val rd_csr_num = Flipped(Valid(UInt(SZ_CSR_NUM.W)))
    val wr_csr_num = Flipped(Valid(UInt(SZ_CSR_NUM.W)))
    val wdata = Input(UInt(32.W))
    val rdata = Output(UInt(32.W))
    val ctrl = new CSRCtrlSignals
  })

  val plv = RegInit(0.U(2.W))
  val ie = RegInit(0.B)
  val da = RegInit(1.B)
  val pg = RegInit(0.B)
  val datf = RegInit(0.U(2.W))
  val datm = RegInit(0.U(2.W))
  val crmd    = Cat(0.U(23.W), datm, datf, pg, da, ie, plv)

  val pplv = Reg(UInt(2.W))
  val pie = Reg(Bool())
  val prmd    = Cat(0.U(29.W), pplv, pie)

  val fpe = RegInit(0.B)
  val euen    = Cat(0.U(31.W), fpe)

  val lie9_0 = RegInit(0.U(10.W))
  val lie12_11 = RegInit(0.U(2.W))
  val lie = Cat(lie12_11, 0.B, lie9_0)
  val ecfg    = Cat(0.U(19.W), lie)

  val swis = RegInit(0.U(2.W))
  val hwis = Reg(UInt(8.W))
  val tis = Reg(Bool())
  val ipis = Reg(Bool())
  val is = Cat(ipis, tis, 0.B, hwis, swis)
  val ecode = Reg(UInt(6.W))
  val esubcode = Reg(UInt(9.W))
  val estat   = Cat(0.B, esubcode, ecode, 0.U(3.W), is)

  val era     = Reg(UInt(32.W))

  val badv    = Reg(UInt(32.W))

  val eentry_va = Reg(UInt(26.W))
  val eentry  = Cat(eentry_va, 0.U(6.W))

  val cpuid   = Cat(0.U(23.W), 0.U(9.W))

  val save0   = Reg(UInt(32.W))
  val save1   = Reg(UInt(32.W))
  val save2   = Reg(UInt(32.W))
  val save3   = Reg(UInt(32.W))

  val rollb = Reg(Bool())
  val wcllb = 0.B
  val klo = RegInit(0.B)
  val llbctl  = Cat(0.U(29.W), klo, wcllb, rollb)

  val tid     = Reg(UInt(32.W))

  val tcfg_en = RegInit(0.B)
  val periodic = Reg(Bool())
  val initval = Reg(UInt(30.W))
  val tcfg    = Cat(initval, periodic, tcfg_en)

  val tval    = Reg(UInt(32.W))

  val ticlr_clr = 0.B
  val ticlr   = Cat(0.U(31.W), ticlr_clr)

  //RD
  val regfile = VecInit(Seq(0.U(32.W), crmd, prmd, euen,
    ecfg, estat, era, badv, eentry,
    cpuid, save0, save1, save2, save3,
    tid, tcfg, tval, ticlr, llbctl))
  val rd_lookup = Cat(Seq(CRMD, PRMD, EUEN,
    ECFG, ESTAT, ERA, BADV, EENTRY,
    CPUID, SAVE0, SAVE1, SAVE2, SAVE3,
    TID, TCFG, TVAL, TICLR, LLBCTL).map(io.rd_csr_num.bits === _))
  rd_lookup := Cat(rd_lookup, !rd_lookup.orR)

  io.rdata := Mux(io.rd_csr_num.valid, 0.U, Mux1H(rd_lookup, regfile))

  io.ctrl.int := crmd(2) && (ecfg(12, 0) & estat(12, 0)).orR

  //WR
  //CRMD
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === CRMD){
    plv := io.wdata(1, 0)
    ie := io.wdata(2)
    da := io.wdata(3)
    pg := io.wdata(4)
    datf := io.wdata(6, 5)
    datm := io.wdata(8, 7)
  }

  //TODO: ertn

  //PRMD
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === PRMD){
    pplv := io.wdata(1, 0)
    pie := io.wdata(2)
  }

  //ECFG
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === ECFG){
    lie9_0 := io.wdata(9, 0)
    lie12_11 := io.wdata(12, 11)
  }

  //ESTAT
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === ESTAT){
    swis := io.wdata(1, 0)
  }
  hwis := io.ctrl.hwi
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === TICLR && io.wdata(0)) {
    tis := 0.B
  }.elsewhen(tcfg_en && tval === 0.U){
    tis := 1.B
  }
  ipis := io.ctrl.ipi

  //TODO: ecode

  //ERA
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === ERA){
    era := io.wdata
  }

  //BADV
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === BADV){
    badv := io.wdata
  }

  //EENTRY
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === EENTRY){
    eentry_va := io.wdata(31, 6)
  }

  //SAVE
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === SAVE0){
    badv := io.wdata
  }
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === SAVE1){
    badv := io.wdata
  }
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === SAVE2){
    badv := io.wdata
  }
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === SAVE3){
    badv := io.wdata
  }

  //TID
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === TID){
    tid := io.wdata
  }

  //TCFG & TVAL
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === TCFG){
    tcfg_en := io.wdata(0)
    periodic := io.wdata(1)
    initval := io.wdata(31, 2)

    tval := Cat(io.wdata(31, 2), 0.U(2.W))
  }.elsewhen(tcfg_en){
    when(tval === 0xffffffff.U){
      tval := tval
    }.elsewhen(tval === 0.U) {
      tval := Mux(tcfg(1), Cat(tcfg(31, 2), 0.U(2.W)), 0xffffffff.U)
    }.otherwise {
      tval := tval - 1.U
    }
  }

  //LLBCTL
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === LLBCTL){
    klo := io.wdata(2)
  }
  when(io.wr_csr_num.valid && io.wr_csr_num.bits === LLBCTL && io.wdata(1)) {
    rollb := 0.B
  }
}
