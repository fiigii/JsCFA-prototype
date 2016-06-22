/**
  * Created by Fei Peng on 6/21/16.
  */
import AAM.State
object AAMHelper {
  def isComplete(ast: AbstractSyntaxTree): Boolean = ast match {
    //Statement
    case EmptyStmt() => true

    //Expressions
    case EmptyExpr() => true
    case _: FunctionExpr => true
    case VarRef(_) => true
    case ThisRef() => true
    case NullLit() => true
    case BoolLit(_) => true
    case NumberLit(_) => true
    case StringLit(_) => true
    case _: RegExp => true

    //LValue
    case LVarRef(_) => true

    //ForInInit
    case _: ForInVarDecl => true
    case _: ForInLValue => true

    //Property
    case _: Property => true

    //Continuation
    case _: KReturnComplete => true
    case _: KIfComplete => true
    case _: KWhileComplete => true
    case _: KForInComplete => true
    case _: KForComplete => true
    case _: KForCompleteTrue => true

    case _: KDotRefComplete => true
    case _: KBracketComplete => true
    case _: KMethodCallComplete => true
    case _: KFuncCallComplete => true
    case _: KNewCallComplete => true
    case _: KAssignExprComplete => true
    case _: KUnaryAssignComplete => true
    case _: KPrefixExprComplete => true
    case _: KInfixExprComplete => true
    case KList(l :: Nil) => true
    case _: KObjectComplete => true
    //case _ : KObjectPairPack => true
    case _: KArrayComplete => true
    case _: KVarDeclStmtComplete => true
    case _: KFunctionDeclComplete => true
    case _: KLDotRefComplete => true
    case _: KLBracketComplete => true

    //otherwise
    case _ => false
  }

  def isCallBuiltIn(func : JSObject) : Boolean = func.builtIn != null

}
