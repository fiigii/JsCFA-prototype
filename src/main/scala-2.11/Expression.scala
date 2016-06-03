import AssignOp.AssignOp
import InfixOp.InfixOp
import PrefixOp.PrefixOp
import UnaryAssignOp.UnaryAssignOp

/**
  * Created by Fei Peng on 2/16/16.
  */
sealed abstract class Expression extends AbstractSyntaxTree

case class EmptyExpr() extends Expression
case class FunctionExpr(name : Option[IntroduceVar], ps : List[IntroduceVar], body : Statement) extends Expression with ObjectGeneratePoint{
  var sourceCode : String = ""
  var prototypeID : Label = -1
}
case class VarRef(name : String) extends Expression with VariableAccess
case class ThisRef() extends Expression
case class DotRef(obj : Expression, prop : String) extends Expression
case class BracketRef(obj : Expression, prop : Expression) extends Expression
case class MethodCall(receiver : Expression, method : Expression, args : List[Expression]) extends Expression
case class FuncCall(func : Expression, args : List[Expression]) extends Expression
case class NewCall(constructor : Expression, args : List[Expression]) extends Expression with ObjectGeneratePoint
case class AssignExpr(op : AssignOp, lv : LValue, expr : Expression) extends Expression
case class NullLit() extends Expression
case class BoolLit(value : Boolean) extends Expression
case class NumberLit(value : Double) extends Expression
case class StringLit(value : String) extends Expression
case class RegExp(regexp : String, global : Boolean, case_insensitive : Boolean) extends Expression with ObjectGeneratePoint
case class ObjectLit(obj : List[ObjectPair]) extends Expression with ObjectGeneratePoint
case class ArrayLit(vs : List[Expression]) extends Expression with ObjectGeneratePoint
case class UnaryAssignExpr(op : UnaryAssignOp, lv: LValue) extends Expression
case class PrefixExpr(op : PrefixOp, expr : Expression) extends Expression
case class InfixExpr(op : InfixOp, expr1 : Expression, expr2 : Expression) extends Expression
case class CondExpr(cond : Expression, thenPart : Expression, elsePart : Expression) extends Expression
case class ListExpr(exprs : List[Expression]) extends Expression


object AssignOp extends Enumeration {
  type AssignOp = Value
  val OpAssign, OpAssignAdd, OpAssignSub, OpAssignMul, OpAssignDiv, OpAssignMod, OpAssignLShift,
      OpAssignSpRShift, OpAssignZfRShift, OpAssignBAnd, OpAssignBXor, OpAssignBOr = Value
}

object UnaryAssignOp extends Enumeration {
  type UnaryAssignOp = Value
  val PrefixInc, PrefixDec, PostfixInc, PostfixDec = Value
}

object PrefixOp extends Enumeration {
  type PrefixOp = Value
  val PrefixLNot, PrefixBNot, PrefixPlus, PrefixMinus, PrefixTypeof, PrefixVoid, PrefixDelete = Value
}

object InfixOp extends Enumeration {
  type InfixOp = Value
  val OpLT, OpLEq, OpGT, OpGEq, OpIn, OpInstanceof, OpEq, OpNEq, OpStrictEq, OpStrictNEq,
      OpLAnd, OpLOr, OpMul, OpDiv, OpMod, OpSub,OpLShift, OpSpRShift, OpZfRShift,
      OpBAnd, OpBXor, OpBOr, OpAdd = Value
}