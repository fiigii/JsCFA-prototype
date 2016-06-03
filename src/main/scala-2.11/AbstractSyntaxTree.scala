
/**
  * Created by Fei Peng on 2/29/16.
  */

trait AbstractSyntaxTree {
  type Label = Long
  var id : Label = -1
  var sourcePosition : SourcePos = null

  def hasID = id != -1
  def generateFrom(ast : AbstractSyntaxTree): Unit = {
    if(!ast.hasID) throw new RuntimeException("Cannot Copy invalid ID.")
    id = ast.id
    sourcePosition = ast.sourcePosition
    this match {
      case gp : ObjectGeneratePoint =>
        gp.protoID = ast.asInstanceOf[ObjectGeneratePoint].protoID
        gp.constructorID = ast.asInstanceOf[ObjectGeneratePoint].constructorID
      case _ =>
    }
  }
}

object AbstractSyntaxTree {
  type Label = Long
  var idSeed: Label = 0
  def freshID = {
    idSeed += 1
    idSeed
  }
}

trait VariableAccess {
  var referTo : IntroduceVar = null
  //override def toString = "refer to: " + referTo  + "\n"
}

trait ObjectGeneratePoint {
  var protoID : AbstractSyntaxTree.Label = -1
  var constructorID : AbstractSyntaxTree.Label = -1
}

case class IntroduceVar(str : String) extends AbstractSyntaxTree

case class SourcePos(sourceName : String, line : Int, column : Int, length: Int)
