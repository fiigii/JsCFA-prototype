import java.io.File

/**
  * Created by Fei Peng on 6/8/16.
  */
object Main {
  def main(args: Array[String]) {
    val ast = GenerateAST(new File("benchmark/sunspider-3d-raytrace.js"))
    //val ast = GenerateAST(new File("test.js"))
    val decedAST = DecorateAST(ast)
    NameResolver(decedAST)
    val disk = AAM.analyze(decedAST)
    /*
    println("AST :")
    DecorateAST.mapToAST.foreach{
      case (id, t) => println(id + " :: " + t)
    }
    */

    println("\n\nResult : ")
    DecorateAST.mapToAST.foreach {
      case (id, v) => if (id > 0 && v.isInstanceOf[IntroduceVar] && disk.contains(JSReference(id))) {
        val values = disk(JSReference(id))
        println(v.asInstanceOf[IntroduceVar].str + " -> ")
        values.foreach[Unit](x => println("    " + x))
      }
    }

  }
}
