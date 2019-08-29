package Ex0

import chisel3._
import chisel3.util._
import chisel3.util.Counter
import chisel3.experimental.MultiIOModule
import chisel3.util.Enum



class FSM (val rowDims: Int, val colDims: Int) extends MultiIOModule {

  val io = IO(
    new Bundle {
      val aRowIdx      = Output(UInt(32.W))
      val aColIdx      = Output(UInt(32.W))
      val aWriteEnable = Output(Bool())
      val bRowIdx      = Output(UInt(32.W))
      val bColIdx      = Output(UInt(32.W))
      val bWriteEnable = Output(Bool())

      val executing    = Output(Bool())
    }
  )

  val debug = IO (
   new Bundle {
     // val aRowIdx = Input(UInt(32.W))
     // val aColIdx = Input(UInt(32.W))
     // val bRowIdx = Input(UInt(32.W))
     // val bColIdx = Input(UInt(32.W))

     val state   = Output(UInt(3.W))
     // val counter = Output(UInt(32.W))
    }
  )


  /**
    * Your code here
    */

  /////////////////////////
  // Create state signal //
  /////////////////////////
  val sReset :: sLoad :: sExecute :: Nil = Enum(3)

  val state = RegInit(sLoad)

  /////////////////////
  // Create counter  //
  /////////////////////
  val w_enableCounter = WireInit(Bool(), false.B)
  val (counter, counterWrap) = Counter(w_enableCounter, colDims)

  // ////////////////////////////////
  // // Create registers and wires //
  // ////////////////////////////////
  val w_controlARowIdx   = WireInit(UInt(32.W), 0.U)
  val w_controlAColIdx   = WireInit(UInt(32.W), 0.U)
  val r_controlAWE       = RegInit(Bool(), true.B)
  val w_controlBRowIdx   = WireInit(UInt(32.W), 0.U)
  val w_controlBColIdx   = WireInit(UInt(32.W), 0.U)
  val r_controlBWE       = RegInit(Bool(), true.B)
  val r_controlExecuting = WireInit(Bool(), true.B)

  val r_wrapCounter      = RegInit(UInt(32.W), 0.U)



  /////////////////
  // Control FSM //
  /////////////////
  switch ( state ){
    is ( sReset ){
      //reset signals
      w_enableCounter      := false.B

      w_controlARowIdx   := 0.U
      w_controlAColIdx   := 0.U
      w_controlBRowIdx   := 0.U
      w_controlBColIdx   := 0.U
      r_controlExecuting := false.B

      // Go to state sLoad
      state              := sLoad
    }

    /**Load Matrices*/
    is ( sLoad ){
      //enable counter
      w_enableCounter      := true.B

      w_controlARowIdx   := r_wrapCounter
      //increment A column every clock cycle
      w_controlAColIdx   := counter

      //increment B's row every clock cycle
      w_controlBRowIdx   := counter
      w_controlBColIdx   := r_wrapCounter

      //Increment B's column when counter wraps (a row is done)
      when (counterWrap){
        // w_controlBColIdx := w_controlBColIdx + 1.U
        r_wrapCounter    := r_wrapCounter + 1.U
       }.otherwise{
        // w_controlBColIdx := w_controlBColIdx
        r_wrapCounter    := r_wrapCounter
      }

      // Enable WriteEnable
      r_controlAWE       := true.B
      r_controlBWE       := true.B

      // Executing
      r_controlExecuting := true.B

      when (r_wrapCounter === rowDims.U - 1.U && (counter === colDims.U - 1.U) ){
        r_wrapCounter      := 0.U
        state              := sExecute
      }.otherwise{
        state              := sLoad
      }
    }

    /**Push correct matrix number to the dot product module*/
    is ( sExecute ){
      //enable counter
      w_enableCounter      := true.B

      w_controlARowIdx   := r_wrapCounter
      //increment A column every clock cycle
      w_controlAColIdx   := counter

      //increment B's row every clock cycle
      w_controlBRowIdx   := counter
      w_controlBColIdx   := r_wrapCounter

      //Increment B's column when counter wraps (a row is done)
      when (counterWrap){
        // w_controlBColIdx := w_controlBColIdx + 1.U
        r_wrapCounter    := r_wrapCounter + 1.U
       }.otherwise{
        // w_controlBColIdx := w_controlBColIdx
        r_wrapCounter    := r_wrapCounter
      }

      // Disable writeEnable
      r_controlAWE       := false.B
      r_controlBWE       := false.B

      when(counter === colDims.U - 1.U){
        r_controlExecuting := false.B
      }.otherwise{
        r_controlExecuting := true.B
      }

      when ((r_wrapCounter === rowDims.U - 1.U) && (counter === colDims.U - 1.U) ){
        // Reset wrapcounter
        r_wrapCounter      := 0.U

        // Stay in state
        state              := sLoad
      }.otherwise{

        // Go to load state
        state              := sExecute
      }
    }
  }


  // Connect output signals
  io.aRowIdx      := w_controlARowIdx
  io.aColIdx      := w_controlAColIdx
  io.aWriteEnable := r_controlAWE
  io.bRowIdx      := w_controlBRowIdx
  io.bColIdx      := w_controlBColIdx
  io.bWriteEnable := r_controlBWE
  io.executing    := r_controlExecuting

  debug.state := state
}
