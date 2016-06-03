/**
  * Created by Fei Peng on 2/22/16.
  */
sealed abstract class ForInit extends AbstractSyntaxTree

case class NoneInit() extends ForInit
case class VarListInit(vars: VarDeclListStmt) extends ForInit
case class VarInit(varDecl: VarDeclStmt) extends ForInit
case class ExprInit(expr: Expression) extends ForInit