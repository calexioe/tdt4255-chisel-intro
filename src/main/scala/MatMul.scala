package Ex0

import chisel3._
import chisel3.experimental.MultiIOModule



class MatMul(val rowDimsA: Int, val colDimsA: Int) extends MultiIOModule {

  val io = IO(
    new Bundle {
      val dataInA     = Input(UInt(32.W))
      val dataInB     = Input(UInt(32.W))

      val dataOut     = Output(UInt(32.W))
      val outputValid = Output(Bool())
    }
  )

  val debug = IO(
    new Bundle {
      val aRowIdx = Output(UInt(32.W))
      val aColIdx = Output(UInt(32.W))
      val aWE     = Output(UInt(32.W))
      val bRowIdx = Output(UInt(32.W))
      val bColIdx = Output(UInt(32.W))
      val bWE     = Output(UInt(32.W))

      val a = Output(UInt(32.W))
      val aIn = Output(UInt(32.W))
      val b = Output(UInt(32.W))

      val outData = Output(UInt(32.W))

      val executing = Output(UInt(32.W))
    }
  )


  /**
    * Your code here
    */

  //////////////////
  // Create wires //
  //////////////////
  val w_validMux         = WireInit(Bool(), true.B)
  val w_dotProdValid     = WireInit(Bool(), false.B)
  val w_dotProdDataOut   = WireInit(UInt(32.W), 0.U)

  val w_matrixADataOut   = WireInit(UInt(32.W), 0.U)
  val w_matrixBDataOut   = WireInit(UInt(32.W), 0.U)


  /////////////////////////////////////////////////////////////////////
  // Create and connect the FSM, 2 matrices and a dot product module //
  /////////////////////////////////////////////////////////////////////
  val fsm         = Module(new FSM(rowDimsA, colDimsA)).io

  val matrixA     = Module(new Matrix(rowDimsA, colDimsA))
  // Inputs
  matrixA.io.dataIn          := io.dataInA
  matrixA.io.rowIdx          := fsm.aRowIdx
  matrixA.io.colIdx          := fsm.aColIdx
  matrixA.io.writeEnable     := fsm.aWriteEnable

  // Outputs
  w_matrixADataOut           := matrixA.io.dataOut

  //Debug
  debug.aIn                  := matrixA.debug.dataIn

  val matrixB     = Module(new Matrix(rowDimsA, colDimsA)).io
  // Inputs
  matrixB.dataIn          := io.dataInB
  matrixB.rowIdx          := fsm.bRowIdx
  matrixB.colIdx          := fsm.bColIdx
  matrixB.writeEnable     := fsm.bWriteEnable
  
  // Outputs
  w_matrixBDataOut        := matrixB.dataOut 

  val dotProdCalc = Module(new DotProd(colDimsA)).io
  // Inputs
  dotProdCalc.dataInA     := w_matrixADataOut
  dotProdCalc.dataInB     := w_matrixBDataOut

  // Outputs
  w_dotProdDataOut        := dotProdCalc.dataOut    
  w_dotProdValid          := dotProdCalc.outputValid 

  ////////////////////
  // Generate logic //
  ////////////////////

  // generate mux for valid signal
  when ( fsm.executing === 0.U){
    w_validMux := w_dotProdValid
  }.otherwise{
    w_validMux := false.B
  }


  /////////////////////
  // Connect output  //
  /////////////////////
  io.dataOut     := w_dotProdDataOut
  io.outputValid := w_validMux

  debug.aRowIdx  := fsm.aRowIdx
  debug.aColIdx  := fsm.aColIdx
  debug.aWE      := fsm.aWriteEnable
  debug.bRowIdx  := fsm.bRowIdx
  debug.bColIdx  := fsm.bColIdx
  debug.bWE      := fsm.bWriteEnable


  debug.a := w_matrixADataOut
  debug.b := w_matrixBDataOut

  debug.outData  := w_dotProdDataOut

  debug.executing := fsm.executing

}
    
