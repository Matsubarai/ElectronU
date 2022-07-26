package core

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import org.scalatest.matchers.should.Matchers
import core.Instructions._

class coreSpec extends AnyFreeSpec with ChiselScalatestTester with Matchers{
  "RAW test" in {
    test(new ElectronCore()){ c =>
      val instr_seq = Seq(
        twoRegImmT(ADDI_W, 1, 0, 2),
        twoRegImmT(ADDI_W, 2, 0, 3),
        threeRegT(SUB_W, 2, 3, 2),
        threeRegT(ADD_W, 4, 2, 4)
      )

      for (_ <- 0 until 5){
        for (i <- instr_seq) {
          c.clock.step(1)
          c.io.imem.rdata.poke(i)
        }
      }

      c.clock.step(1)
      c.io.imem.rdata.poke(ADDI_W.value)
      c.clock.step(5)
    }
  }

  "LD test" in {
    test(new ElectronCore()){ c =>
      val instr_seq = Seq(
        twoRegImmT(LD_HU, 2, 0, 2),
        twoRegImmT(LD_HU, 4, 0, 3),
//        immT(B, 2),
//        immT(BL, 2),
//        twoRegImmT(JIRL, 3, 3, 5),
//        twoRegImmT(BGE, 2, 2, 3),
//        twoRegImmT(BGEU, 2, 2, 3),
        threeRegT(ADD_W, 2, 3, 2),
        threeRegT(ADD_W, 4, 2, 4)
      )
      val data_seq = Seq((BigInt(1) << 32) -2, BigInt(3))

      for (_ <- 0 until 25){
        val x = c.io.imem.addr.peekInt().toInt
        val y = c.io.dmem.addr.peekInt().toInt
        val en = c.io.dmem.en.peekBoolean()
        c.clock.step(1)
        if(en)
          c.io.dmem.rdata.poke(data_seq(y).U)
        c.io.imem.rdata.poke(instr_seq(x % instr_seq.length))
      }

      c.clock.step(1)
      c.io.imem.rdata.poke(ADDI_W.value)
      c.clock.step(5)
    }
  }
  "DIV MOD test" in {
    test(new ElectronCore()){ c =>
      val instr_seq = Seq(
        twoRegImmT(ADDI_W, -4, 0, 1),
        twoRegImmT(ADDI_W, 2, 0, 2),
        threeRegT(DIV_W, 2, 1, 3),
        threeRegT(MOD_W, 2, 1, 4)
      )

      for (_ <- 0 until 5){
        for (i <- instr_seq) {
          c.clock.step(1)
          c.io.imem.rdata.poke(i.U)
        }
      }

      c.clock.step(1)
      c.io.imem.rdata.poke(ADDI_W.value)
      c.clock.step(5)
    }
  }
  "logic func test" in {
    test(new ElectronCore()){ c =>
      val instr_seq = Seq(
        twoRegImmT(LD_W, 0, 0, 2),
        twoRegImmT(LD_W, 4, 0, 3),
        threeRegT(SLT, 3, 2, 4),
        threeRegT(SLTU, 3, 2, 4),
        threeRegT(SLL_W, 3, 2, 4),
        threeRegT(SRL_W, 3, 2, 4),
        threeRegT(SRA_W, 3, 2, 4),
        oneRegImmT(LU12I_W, 1, 4),
        oneRegImmT(PCADDU12I, 1, 4),
        threeRegT(NOR, 3, 2, 4),
        threeRegT(AND, 3, 2, 4),
        threeRegT(OR, 3, 2, 4),
        threeRegT(XOR, 3, 2, 4),
      )
      val data_seq = Seq((BigInt(1) << 32) -2, BigInt(3))

      for (_ <- 0 until 25){
        val x = c.io.imem.addr.peekInt().toInt
        val y = c.io.dmem.addr.peekInt().toInt
        val en = c.io.dmem.en.peekBoolean()
        c.clock.step(1)
        if(en)
          c.io.dmem.rdata.poke(data_seq(y).U)
        c.io.imem.rdata.poke(instr_seq(x % instr_seq.length))
      }

      c.clock.step(1)
      c.io.imem.rdata.poke(ADDI_W.value)
      c.clock.step(5)
    }
  }
}
