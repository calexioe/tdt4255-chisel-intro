package Ex0

import chisel3._
import chisel3.iotesters.PeekPokeTester
import org.scalatest.{Matchers, FlatSpec}
import TestUtils._

class MatMulSpec extends FlatSpec with Matchers {
  import MatMulTests._

  val rowDims = 3
  val colDims = 7





  behavior of "MatMulFsm"

  it should "Test FSM" in {
    wrapTester(
      chisel3.iotesters.Driver(() => new FSM(rowDims, colDims), verbose = false) { a =>
        new FsmTest(a)
      } should be(true)
    )
  }

  behavior of "MatMul"

  it should "Update matrix A" in {
    wrapTester(
      chisel3.iotesters.Driver(() => new MatMul(rowDims, colDims), verbose = false) { c =>
        new UpdateMatrix(c)
      } should be(true)
    )
  }


  behavior of "MatMul"

  it should "Do shit" in {
    wrapTester(
      chisel3.iotesters.Driver(() => new MatMul(rowDims, colDims), verbose = true) { c =>
        new FullMatMul(c)
      } should be(true)
    )
  }
}

object MatMulTests {

  val rand = new scala.util.Random(100)

  class FsmTest(a:FSM) extends PeekPokeTester(a) {

    //Test initialization of row and cols
    //expect(a.io.aRowIdx, 0,  "aRowIdx is wrong")
    //expect(a.io.aColIdx, 0,  "aColIdx is wrong")
    //expect(a.io.bRowIdx, 0,  "bRowIdx is wrong")
    //expect(a.io.bColIdx, 0,  "bColIdx is wrong")

    //Go from reset state to load state
    //step (1)

    //Test a 5x5 matrix 3 times
    for (j <- 0 until 3){
      for(i <- 0 until 3){
        for(ii <- 0 until 7){
          peek(a.debug.state)

          peek(a.io.aRowIdx)
          peek(a.io.aColIdx)
          peek(a.io.aWriteEnable)
          peek(a.io.bRowIdx)
          peek(a.io.bColIdx)
          peek(a.io.bWriteEnable)
          peek(a.io.executing)

          expect(a.io.aRowIdx, i)
          expect(a.io.aColIdx, ii)
          expect(a.io.bRowIdx, ii)
          expect(a.io.bColIdx, i)

          // Check timing of execute signal
          if((peek(a.debug.state) == 2.U) &&  ii == 6){
            expect(a.io.executing, 0)
          }
          step(1)
        }
      }
    }
  }


  class UpdateMatrix(c: MatMul) extends PeekPokeTester(c) {
    val mA = genMatrix(c.rowDimsA, c.colDimsA)
    val mB = genMatrix(c.rowDimsA, c.colDimsA)
    val mC = matrixMultiply(mA, mB.transpose)

    // Input data

    for(ii <- 0 until c.colDimsA * c.rowDimsA){

      val rowInputIdx = ii / c.colDimsA
      val colInputIdx = ii % c.colDimsA

      poke(c.io.dataInA, mA(rowInputIdx)(colInputIdx))
      poke(c.io.dataInB, 0.U)
      peek(c.debug.aIn)

      peek(c.debug.aRowIdx)
      peek(c.debug.aColIdx)
      peek(c.debug.aWE)

      peek(c.debug.a)
      peek(c.debug.b)

      step(1)
    }

    //read back data
    for(ii <- 0 until c.colDimsA * c.rowDimsA){

      val rowInputIdx = ii / c.colDimsA
      val colInputIdx = ii % c.colDimsA

      expect(c.debug.a, mA(rowInputIdx)(colInputIdx))
      step(1)
    }
  }


  class FullMatMul(c: MatMul) extends PeekPokeTester(c) {

    val mA = genMatrix(c.rowDimsA, c.colDimsA)
    val mB = genMatrix(c.rowDimsA, c.colDimsA)
    val mC = matrixMultiply(mA, mB.transpose)

    println("Multiplying")
    println(printMatrix(mA))
    println("With")
    println(printMatrix(mB.transpose))
    println("Expecting")
    println(printMatrix(mC))

    // Input data
    for(ii <- 0 until c.colDimsA * c.rowDimsA){

      val rowInputIdx = ii / c.colDimsA
      val colInputIdx = ii % c.colDimsA

      poke(c.io.dataInA, mA(rowInputIdx)(colInputIdx))
      poke(c.io.dataInB, mB(rowInputIdx)(colInputIdx))

      peek(c.debug.aRowIdx)
      peek(c.debug.aColIdx)
      peek(c.debug.aWE)
      peek(c.debug.bRowIdx)
      peek(c.debug.bColIdx)
      peek(c.debug.bWE)

      peek(c.debug.a)
      peek(c.debug.b)

      peek(c.debug.executing)
      peek(c.io.outputValid)
      expect(c.io.outputValid, false, "Valid output during initialization")

      step(1)
    }

    // Perform calculation
    for(ii <- 0 until (c.rowDimsA * c.rowDimsA)){
      for(kk <- 0 until c.colDimsA - 1){
        peek(c.debug.aRowIdx)
        peek(c.debug.aColIdx)
        peek(c.debug.bRowIdx)
        peek(c.debug.bColIdx)

        peek(c.debug.a)
        peek(c.debug.b)

        peek(c.debug.executing)
        peek(c.debug.outData)
        peek(c.io.outputValid)

        expect(c.io.outputValid, false, "Check correct timing")
        step(1)
      }
      peek(c.debug.aRowIdx)
      peek(c.debug.aColIdx)
      peek(c.debug.bRowIdx)
      peek(c.debug.bColIdx)

      peek(c.debug.a)
      peek(c.debug.b)

      peek(c.debug.executing)
      peek(c.debug.outData)
      peek(c.io.outputValid)

      expect(c.io.outputValid, true, "Check valid output timing")
      expect(c.io.dataOut, mC(ii / c.rowDimsA)(ii % c.rowDimsA), "Check correct value calculated")
      step(1)
    }
  }
}
