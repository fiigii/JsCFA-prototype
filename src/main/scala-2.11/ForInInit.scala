/**
  * Created by dearkx on 2/16/16.
  */
sealed abstract class ForInInit extends AbstractSyntaxTree

case class ForInVarDecl(name : IntroduceVar) extends ForInInit()
case class ForInLValue(lVal : LValue) extends ForInInit()