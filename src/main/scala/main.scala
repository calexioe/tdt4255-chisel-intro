package Ex0
import chisel3._
import java.io.File

//Import sys.process to be able to run shell commands
import scala.sys.process._
import sys.process._

object main {
  def main(args: Array[String]): Unit = {
    val s = """
    | Attempting to "run" a chisel program is rather meaningless.
    | Instead, try running the tests, for instance with "test" or "testOnly Examples.MyIncrementTest
    | 
    | If you want to create chisel graphs, simply remove this message and comment in the code underneath 
    | to generate the modules you're interested in.
    """.stripMargin
    println(s)

    if (args.length != 0){
      if ( args(0) == "-d" ){
        // Uncomment to dump .fir file
        val component = "matmul"
        val f = new File(s"./output_products/fir/${component}.fir")
        chisel3.Driver.dumpFirrtl(chisel3.Driver.elaborate(() => new MatMul(3, 7)), Option(f))

        //run diagrammer

        val path = "pwd".!!.trim
        println(path)
        s"$path/src/main/scala/make_diagram.sh $component $path".!
      }
    }
  }
}
