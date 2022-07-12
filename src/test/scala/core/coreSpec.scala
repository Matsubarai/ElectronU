package core

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import org.scalatest.matchers.should.Matchers
import core.Instructions._

class coreSpec extends AnyFreeSpec with ChiselScalatestTester with Matchers{
  "WAR test" in {
    test(new ElectronCore()){ c =>
      val instr_seq = Seq(
        twoRegImmT(ADDI_W, 1, 0, 2),
        twoRegImmT(ADDI_W, 2, 0, 3),
        threeRegT(ADD_W, 2, 3, 2),
        threeRegT(ADD_W, 4, 2, 4)
      )

      for (t <- 0 until 10){
        for (i <- instr_seq.indices) {
          c.clock.step(1)
          c.io.imem.rdata.poke(instr_seq(i))
          println(c.io.pc.peek())
          println(instr_seq(i))
        }
      }

      c.clock.step(1)
      c.io.imem.rdata.poke(ADDI_W.value)
      c.clock.step(10)
    }
  }
}
