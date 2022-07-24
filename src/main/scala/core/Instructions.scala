package core

import chisel3._
import chisel3.util._

object Instructions {
  def ADD_W   = BitPat("b00000000000100000???????????????")
  def ADDI_W  = BitPat("b0000001010??????????????????????")
  def SUB_W   = BitPat("b00000000000100010???????????????")

  def LD_W    = BitPat("b0010100010??????????????????????")
  def ST_W    = BitPat("b0010100110??????????????????????")

  def BEQ     = BitPat("b010110??????????????????????????")
  def BNE     = BitPat("b010111??????????????????????????")
  def BLT     = BitPat("b011000??????????????????????????")
  def BGE     = BitPat("b011001??????????????????????????")
  def BLTU    = BitPat("b011010??????????????????????????")
  def BGEU    = BitPat("b011011??????????????????????????")

  def JIRL    = BitPat("b010011??????????????????????????")
  def B       = BitPat("b010100??????????????????????????")
  def BL      = BitPat("b010101??????????????????????????")

  def SLT     = BitPat("b00000000000100100???????????????")
  def SLTU    = BitPat("b00000000000100101???????????????")
  def SLTI    = BitPat("b0000001000??????????????????????")
  def SLTUI   = BitPat("b0000001001??????????????????????")

  def SLL_W   = BitPat("b00000000000101110???????????????")
  def SRL_W   = BitPat("b00000000000101111???????????????")
  def SRA_W   = BitPat("b00000000000110000???????????????")
  def SLLI_W  = BitPat("b00000000010000001???????????????")
  def SRLI_W  = BitPat("b00000000010001001???????????????")
  def SRAI_W  = BitPat("b00000000010010001???????????????")

  def MUL_W   = BitPat("b00000000000111000???????????????")
  def MULH_W  = BitPat("b00000000000111001???????????????")
  def MULH_WU = BitPat("b00000000000111010???????????????")
  def DIV_W   = BitPat("b00000000001000000???????????????")
  def MOD_W   = BitPat("b00000000001000001???????????????")
  def DIV_WU  = BitPat("b00000000001000010???????????????")
  def MOD_WU  = BitPat("b00000000001000011???????????????")

  def LU12I_W = BitPat("b0001010?????????????????????????")
  def NOR     = BitPat("b00000000000101000???????????????")
  def AND     = BitPat("b00000000000101001???????????????")
  def OR      = BitPat("b00000000000101010???????????????")
  def XOR     = BitPat("b00000000000101011???????????????")
  def ANDI    = BitPat("b0000001101??????????????????????")
  def ORI     = BitPat("b0000001110??????????????????????")
  def XORI    = BitPat("b0000001111??????????????????????")

  private def Cat(seq: Seq[BigInt]): BigInt = {
    var ret: BigInt = 0
    var mask: BigInt = (BigInt(1) << 32) - 1
    for (i <- seq) {
      ret <<= 5
      ret |= (i & mask)
      if (mask != (1 << 5) - 1){
        mask = (1 << 5) - 1
      }
    }
    ret
  }

  def instrT(bitPat: BitPat, seq: Seq[BigInt]): BigInt = ~bitPat.mask & (BigInt(1) << 32) - 1 & Cat(seq) | bitPat.value

  def twoRegT(bitPat: BitPat, rj: BigInt, rd: BigInt): BigInt = instrT(bitPat, Seq(rj, rd))
  def threeRegT(bitPat: BitPat, rk: BigInt, rj: BigInt, rd: BigInt): BigInt = instrT(bitPat, Seq(rk, rj, rd))
  def fourRegT(bitPat: BitPat, ra: BigInt, rk: BigInt, rj: BigInt, rd: BigInt): BigInt = instrT(bitPat, Seq(ra, rk, rj, rd))
  def twoRegImmT(bitPat: BitPat, imm: BigInt, rj: BigInt, rd: BigInt): BigInt = instrT(bitPat, Seq(imm, rj, rd))
  def oneRegImmT(bitPat: BitPat, immh: BigInt, rj: BigInt, imml: BigInt): BigInt = instrT(bitPat, Seq(immh, rj, imml))
  def immT(bitPat: BitPat, imm: BigInt): BigInt = instrT(bitPat, Seq(imm))
}
