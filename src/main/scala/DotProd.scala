package Ex0

import chisel3._
import chisel3.util.Counter
import chisel3.experimental.MultiIOModule

class DotProd(val elements: Int) extends MultiIOModule {

  val io = IO(
    new Bundle {
      val dataInA     = Input(UInt(32.W))
      val dataInB     = Input(UInt(32.W))

      val dataOut     = Output(UInt(32.W))
      val outputValid = Output(Bool())
    }
  )


  /**
    * Your code here
    */

  val enableCounter = true.B

  val (counter, counterWrap) = Counter(enableCounter, elements)
  val accumulator = RegInit(UInt(32.W), 0.U)
  val resetAccumulator = WireInit(Bool(), true.B)


  // Please don't manually implement product!
  val product = io.dataInA * io.dataInB

  //set output valid when counter have counted elements times
  when (counterWrap) {
    io.outputValid := true.B
    resetAccumulator := true.B
  }.otherwise{
    io.outputValid := false.B
    resetAccumulator := false.B
  }

  //reset accumulator when finished calculating dot product
  when (resetAccumulator) {
    accumulator := 0.U
  }.otherwise{
    //update accumulator
    accumulator := accumulator + product
  }


  // assign output
  io.dataOut := accumulator + product
}
