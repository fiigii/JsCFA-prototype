/**
  * Created by Fei Peng on 2/16/16.
  */
sealed abstract class LValue extends AbstractSyntaxTree

case class LVarRef(name : String) extends LValue with VariableAccess
case class LDot(obj : Expression, field : String) extends LValue
case class LBracket(obj : Expression, computeField : Expression) extends LValue