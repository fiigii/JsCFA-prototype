
/**
  * Created by Fei Peng on 3/21/16.
  */

abstract sealed class Pattern

case class PConst() extends Pattern
case class PVar(name : Symbol) extends Pattern
case class Destruct(constructor : Symbol, formals : List[Pattern]) extends Pattern
case class Bound(name : Symbol, pattern : Destruct) extends Pattern

abstract sealed class MetaDefinition
case class Eval(parameters: List[Pattern], body : MetaExpression) extends MetaDefinition

abstract sealed class MetaExpression
case class MLet(v : Symbol, init : MetaExpression, body : MetaExpression) extends MetaExpression
case class MVar(name : Symbol) extends MetaExpression
case class MEval(arguments : List[MetaExpression]) extends MetaExpression
case class MLambda(parameters : List[Symbol], body : MetaExpression) extends MetaExpression
case class MApply(func : MetaExpression, arguments : List[MetaExpression]) extends MetaExpression
case class MUnion(res : List[MetaExpression]) extends MetaExpression
case class MCase(expr : MetaExpression, of : List[Of]) extends MetaExpression
case class Of(pattern: Pattern, expr : MetaExpression)
case class ScalaCode(code : String) extends MetaExpression

