/**
  * This code supplements instructions.org
  */
package Examples
import Ex0._


import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

import java.io.File

class SimpleDelay() extends Module {
  val io = IO(
    new Bundle {
      val dataIn  = Input(UInt(32.W))
      val dataOut = Output(UInt(32.W))
    }
  )
  val delayReg = RegInit(UInt(32.W), 0.U)

  delayReg   := io.dataIn
  io.dataOut := delayReg
}


class DelaySpec extends FlatSpec with Matchers {
  behavior of "SimpleDelay"

  it should "Delay input by one timestep" in {
    chisel3.iotesters.Driver(() => new SimpleDelay, verbose = true) { c =>
      new DelayTester(c)
    } should be(true)
  }
}


class DelayTester(c: SimpleDelay) extends PeekPokeTester(c)  {
  for(ii <- 0 until 10){
    val input = scala.util.Random.nextInt(10)
    poke(c.io.dataIn, input)
    step(1)
    expect(c.io.dataOut, input)
  }
}

object Main extends App {
  val f = new File("SimpleDelay.fir")
  chisel3.Driver.dumpFirrtl(chisel3.Driver.elaborate(() => new SimpleDelay), Option(f))
}
