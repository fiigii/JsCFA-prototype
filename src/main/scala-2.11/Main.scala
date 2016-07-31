import java.io.File

/**
  * Created by Fei Peng on 6/8/16.
  */
object Main {
  def main(args: Array[String]) {
    //val ast = GenerateAST(new File("benchmark/sunspider-controlflow-recursive.js"))
    val ast = GenerateAST(new File("test.js"))
    val decedAST = DecorateAST(ast)
    NameResolver(decedAST)
    val t1 = System.currentTimeMillis
    val disk = AAM.analyze(decedAST)
    val t2 = System.currentTimeMillis
    /*
    println("AST :")
    DecorateAST.mapToAST.foreach{
      case (id, t) => println(id + " :: " + t)
    }*/

    println("Disk : ")
    disk.foreach{
      case (id, t) => println(id + " ->")
        for(v <- t) {
          println("    " + v + "\n")
        }
    }

    println("\n\nResult : ")
    DecorateAST.mapToAST.foreach {
      case (id, v) => if (id > 0 && v.isInstanceOf[IntroduceVar] && disk.contains(JSReference(id))) {
        val values = disk(JSReference(id))
        println(v.asInstanceOf[IntroduceVar].str + " -> ")
        values.foreach[Unit](x => println("    " + x))
      }
    }

    println((t2 - t1) + " msecs")
    println("Miss Match: " + AAM.missCount)

  }
}
