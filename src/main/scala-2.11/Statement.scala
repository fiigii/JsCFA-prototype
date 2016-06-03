/**
  * Created by Fei Peng on 2/15/16.
  */
sealed abstract class Statement extends AbstractSyntaxTree

case class Script(stmts : List[Statement]) extends  Statement
case class BlockStmt(stmts : List[Statement]) extends Statement
case class VarDeclListStmt(decls : List[Statement]) extends Statement
case class EmptyStmt() extends Statement
case class ExprStmt(expr : Expression) extends Statement()
case class VarDeclStmt(name : IntroduceVar, expr : Expression) extends Statement
case class FunctionDecl(name : IntroduceVar, fun : Expression) extends Statement
case class ReturnStmt(expr : Expression) extends Statement
case class IfStmt(cond : Expression, thenPart : Statement, elsePart : Statement) extends Statement
case class SwitchStmt(cond : Expression, cases : List[CaseStmt], defaultCase : Option[CaseStmt]) extends Statement
case class CaseStmt(expr : Expression, body : Statement) extends Statement
case class BreakStmt(breakLabel : String) extends Statement
case class ContinueStmt(continueLabel : String) extends Statement
case class DoWhileStmt(cond : Expression, body : Statement) extends Statement
case class WhileStmt(cond : Expression, body : Statement) extends Statement
case class ForStmt(init : ForInit, cond : Option[Expression], increment : Option[Expression], body : Statement) extends Statement
case class ForInStmt(init : ForInInit, expr : Expression, body : Statement) extends Statement
case class LabeledStmt(label : List[String], stmt : Statement) extends Statement
case class TryStmt(body : Statement, catchClause : List[CatchStmt], finalCatch : Option[Statement]) extends Statement
case class CatchStmt(name : IntroduceVar, body : Statement) extends Statement
case class ThrowStmt(expr : Expression) extends Statement
