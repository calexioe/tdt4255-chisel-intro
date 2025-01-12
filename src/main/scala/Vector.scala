package Ex0

import chisel3._


class Vector(val elements: Int) extends Module {

  val io = IO(
    new Bundle {
      val idx         = Input(UInt(32.W))
      val dataIn      = Input(UInt(32.W))
      val writeEnable = Input(Bool())

      val dataOut     = Output(UInt(32.W))
    }
  )

  // Creates a vector of zero-initialized registers
  val internalVector = RegInit(VecInit(List.fill(elements)(0.U(32.W))))


  when(io.writeEnable){
    //Update InternalVector when writeenable is set high
    internalVector(io.idx) := io.dataIn
  }
  //else - do nothing

  // dataout is always internalvector(io.idx)
  io.dataOut := internalVector(io.idx)
}
