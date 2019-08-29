package Ex0

import chisel3._
import chisel3.experimental.MultiIOModule

// This import statement makes the scala vector invisible, reducing confusion
import scala.collection.immutable.{ Vector => _ }

class Matrix(val rowsDim: Int, val colsDim: Int) extends MultiIOModule {

  val io = IO(
    new Bundle {
      val colIdx      = Input(UInt(32.W))
      val rowIdx      = Input(UInt(32.W))
      val dataIn      = Input(UInt(32.W))
      val writeEnable = Input(Bool())

      val dataOut     = Output(UInt(32.W))
    }
  )
  val debug = IO(
    new Bundle {
      val dataIn = Output(UInt(32.W))
    }
  )

  // Creates a vector of the module vector
  val rows = VecInit(Seq.fill(rowsDim)(Module(new Vector(colsDim)).io))

  //Connect wires to vector modules
  for(ii <- 0 until rowsDim){
    // Connect input data
    rows(ii).dataIn      := io.dataIn

    // Create a seperate mux for each row(ii).writeEnable
    // If the rowIdx equals the iterator than let writeEnable through, else set it to false.
    when (ii.U === io.rowIdx) {
      rows(ii).writeEnable := io.writeEnable
    }.otherwise{
      rows(ii).writeEnable := false.B
    }

    // Connect column index
    rows(ii).idx         := io.colIdx
  }

  // Set output
  io.dataOut := rows(io.rowIdx).dataOut

  debug.dataIn := rows(io.rowIdx).dataIn
}
