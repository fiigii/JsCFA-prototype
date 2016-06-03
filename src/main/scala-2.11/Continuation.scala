import AssignOp.AssignOp
import InfixOp.InfixOp
import PrefixOp.PrefixOp
import UnaryAssignOp.UnaryAssignOp

import scala.collection.mutable.Map

/**
  * Created by Fei Peng on 4/5/16.
  */
abstract class Continuation extends AbstractSyntaxTree

case object Halt extends Continuation
case class KScript(after : List[Statement]) extends Continuation
case class KBlockStmt(after : List[Statement]) extends Continuation
case class KVarDeclListStmt(after : List[Statement]) extends Continuation
case class KVarDeclStmt(name : IntroduceVar) extends Continuation
case class KVarDeclStmtComplete(name : IntroduceVar, v : JSValue) extends Continuation
case class KFunctionDeclBody(name : IntroduceVar) extends Continuation
case class KFunctionDeclComplete(name : IntroduceVar, func : JSValue) extends Continuation
case class KReturn() extends Continuation
case class KReturnComplete(v : JSValue) extends Continuation
case class KIfCond(thenPart : Statement, elsePart : Statement) extends Continuation
case class KIfComplete(cond : JSValue, thenPart : Statement, elsePart : Statement) extends Continuation
case class KSwitchE(cases : List[CaseStmt], defaultCase : Option[CaseStmt]) extends Continuation
case class KSwitchCs(cond : JSValue, afterCases : List[CaseStmt], defaultCase : Option[CaseStmt]) extends Continuation
case class KSwitchD(cond : JSValue, afterCases : List[CaseStmt]) extends Continuation
case class KCaseE(body : Statement) extends Continuation
case class KCaseBody(cond : JSValue) extends Continuation
case class KDoWhileE(cond : Expression, body : Statement) extends Continuation
case class KDoWhileBody(cond : Expression, body : Statement) extends Continuation
case class KDoWhileComplete(cond : JSValue, condE : Expression, body : Statement) extends Continuation
case class KWhileE(body : Statement) extends Continuation
case class KWhileComplete(cond : JSValue, body : Statement) extends Continuation
case class KForInE(init : ForInInit, body : Statement) extends Continuation
case class KForInI(expr : JSValue, body : Statement) extends Continuation
case class KForInComplete(init : JSValue, expr : JSValue, body : Statement) extends Continuation
case class KForI(cond : Option[Expression], increment : Option[Expression], body : Statement) extends Continuation
case class KForC(increment : Option[Expression], body : Statement) extends Continuation
case class KForComplete(cond : JSValue, increment : Option[Expression], body : Statement) extends Continuation
case class KForCompleteTrue(increment : Option[Expression], body : Statement) extends Continuation
//TODO: anther Statement (Exception)

// Expressions
case class KDotRef(prop : String) extends Continuation
case class KDotRefComplete(obj : JSValue, prop : String) extends Continuation
case class KBracketO(prop : Expression) extends Continuation
case class KBracketP(obj : JSValue) extends Continuation
case class KBracketComplete(obj : JSValue, prop : JSValue) extends Continuation
case class KMethodCallR(method : Expression, args : List[Expression]) extends Continuation
case class KMethodCallF(receiver : JSValue, args : List[Expression]) extends Continuation
case class KMethodCallA(receiver : JSValue, method : JSValue, before : List[JSValue], after : List[Expression]) extends Continuation
case class KMethodCallComplete(receiver : JSValue, method : JSValue, args : List[JSValue]) extends Continuation
case class KFuncCallF(args : List[Expression]) extends Continuation
case class KFuncCallA(func : JSValue, before : List[JSValue], after : List[Expression]) extends Continuation
case class KFuncCallComplete(func : JSValue, args : List[JSValue]) extends Continuation
case class KNewCallF(args : List[Expression]) extends Continuation with ObjectGeneratePoint
case class KNewCallA(func : JSValue, before : List[JSValue], after : List[Expression]) extends Continuation with ObjectGeneratePoint
case class KNewCallComplete(func : JSValue, args : List[JSValue]) extends Continuation with ObjectGeneratePoint
case class KAssignL(op : AssignOp, expr : JSValue) extends Continuation
case class KAssignR(op : AssignOp, lv : LValue) extends Continuation
case class KAssignExprComplete(op : AssignOp, lv : JSValue, expr : JSValue) extends Continuation
case class KObjectLit(before : Map[JSString, JSValue], after : List[ObjectPair]) extends Continuation with ObjectGeneratePoint
case class KObjectComplete(pairs : Map[JSString, JSValue]) extends Continuation with ObjectGeneratePoint
case class KArrayLit(before : List[JSValue], after : List[Expression]) extends Continuation with ObjectGeneratePoint
case class KArrayComplete(elements : List[JSValue]) extends Continuation with ObjectGeneratePoint
case class KUnaryAssign(op : UnaryAssignOp) extends Continuation
case class KUnaryAssignComplete(op : UnaryAssignOp, lv: JSValue) extends Continuation
case class KPrefix(op : PrefixOp) extends Continuation
case class KPrefixExprComplete(op : PrefixOp, expr : JSValue) extends Continuation
case class KInfixL(op : InfixOp, expr2 : Expression) extends Continuation
case class KInfixR(op : InfixOp, expr1 : JSValue) extends Continuation
case class KInfixExprComplete(op : InfixOp, expr1 : JSValue, expr2 : JSValue) extends Continuation
case class KCondC(thenPart : Expression, elsePart : Expression) extends Continuation
case class KCondComplete(cond: JSValue, thenPart : Expression, elsePart : Expression) extends Continuation
case class KList(after : List[Expression]) extends Continuation

//
case class KUseValue(v : JSValue) extends Continuation


// LValues
case class KLDotRef(prop : String) extends Continuation
case class KLDotRefComplete(obj : JSValue, prop : String) extends Continuation
case class KLBracketO(prop : Expression) extends Continuation
case class KLBracketP(obj : JSValue) extends Continuation
case class KLBracketComplete(obj : JSValue, prop : JSValue) extends Continuation

// Property
case class KObjectPairE(name : JSString) extends Continuation




