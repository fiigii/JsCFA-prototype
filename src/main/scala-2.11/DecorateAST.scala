/**
  * Created by Fei Peng on 3/22/16.
  */
import AbstractSyntaxTree._

object DecorateAST {
  val mapToAST = collection.mutable.Map[Long, AbstractSyntaxTree]()

  def apply(ast : Statement) : Statement = {
    JSBuiltIn.setBuiltIn
    decorateStatement(ast)
  }

  def decorateStatement(stmt : Statement) : Statement ={
    val id = freshID
    stmt match {
      case Script(stmts) =>
        val block = Script(stmts.map(decorateStatement(_)))
        block.id = id
        mapToAST += id -> block
        block

      case BlockStmt(stmts) =>
        val block = BlockStmt(stmts.map(decorateStatement(_)))
        block.id = id
        mapToAST += id -> block
        block

      case VarDeclListStmt(decls) =>
        val declList = VarDeclListStmt(decls.map(decorateStatement(_)))
        declList.id = id
        mapToAST += id -> declList
        declList

      case e@EmptyStmt() =>
        e.id = id
        mapToAST += id -> e
        e

      case ExprStmt(expr) =>
        val e = ExprStmt(decorateExpression(expr))
        e.id = id
        mapToAST += id -> e
        e

      case VarDeclStmt(x, expr) =>
        val varDecl = VarDeclStmt(decorateName(x), decorateExpression(expr))
        varDecl.id = id
        mapToAST += id -> varDecl
        varDecl

      case FunctionDecl(name, func) =>
        val func_ = decorateExpression(func)
        func_ match {
          case FunctionExpr(Some(inName), _, _) =>
            name.id = inName.id
          case FunctionExpr(None, _, _) =>
            val idx = freshID
            name.id = idx
            mapToAST += idx -> name
          case _ => throw new RuntimeException("must be a function")
        }
        val f = FunctionDecl(name, func_)
        f.id = id
        mapToAST += id -> f
        f

      case ReturnStmt(expr) =>
        val ret = ReturnStmt(decorateExpression(expr))
        ret.id = id
        mapToAST += id -> ret
        ret

      case IfStmt(cond, thenPart, elsePart) =>
        val ifS = IfStmt(decorateExpression(cond),
          decorateStatement(thenPart),
          decorateStatement(elsePart))
        ifS.id = id
        mapToAST += id -> ifS
        ifS

      case SwitchStmt(cond, cases, defaultCase) =>
        val sw = SwitchStmt(decorateExpression(cond),
          cases.map(decorateStatement(_)).asInstanceOf[List[CaseStmt]],
          defaultCase.map(decorateStatement(_).asInstanceOf[CaseStmt]))
        sw.id = id
        mapToAST += id -> sw
        sw

      case CaseStmt(expr, body) =>
        val cs = CaseStmt(decorateExpression(expr), decorateStatement(body))
        cs.id = id
        mapToAST += id -> cs
        cs

      case b@BreakStmt(ls) =>
        b.id = id
        mapToAST += id -> b
        b

      case c@ContinueStmt(ls) =>
        c.id = id
        mapToAST += id -> c
        c

      case DoWhileStmt(cond, body) =>
        val doW = DoWhileStmt(decorateExpression(cond), decorateStatement(body))
        doW.id = id
        mapToAST += id -> doW
        doW

      case WhileStmt(cond, body) =>
        val whil = WhileStmt(decorateExpression(cond), decorateStatement(body))
        whil.id = id
        mapToAST += id -> whil
        whil

      case ForStmt(init, cond, inc, body) =>
        val forStmt = ForStmt(decorateForInit(init),
          cond.map(decorateExpression(_)),
          inc.map(decorateExpression(_)),
          decorateStatement(body))
        forStmt.id = id
        mapToAST += id -> forStmt
        forStmt

      case ForInStmt(init, expr, body) =>
        val forIn = ForInStmt(decorateForInInit(init),
          decorateExpression(expr),
          decorateStatement(body))
        forIn.id = id
        mapToAST += id -> forIn
        forIn

      case LabeledStmt(ls, s) =>
        val lstmt = LabeledStmt(ls, decorateStatement(s))
        lstmt.id = id
        mapToAST += id -> lstmt
        lstmt

      case TryStmt(body, clst, finalC) =>
        val tryStmt = TryStmt(decorateStatement(body),
          clst.map(decorateStatement(_)).asInstanceOf[List[CatchStmt]],
          finalC.map(decorateStatement(_)))
        tryStmt.id = id
        mapToAST += id -> tryStmt
        tryStmt

      case CatchStmt(x, body) =>
        val cStmt = CatchStmt(decorateName(x), decorateStatement(body))
        cStmt.id = id
        mapToAST += id -> cStmt
        cStmt

      case ThrowStmt(expr) =>
        val throwStmt = ThrowStmt(decorateExpression(expr))
        throwStmt.id = id
        mapToAST += id -> throwStmt
        throwStmt
    }
  }

  def decorateExpression(theExpr : Expression) : Expression = {
    val id = freshID
    theExpr match {
      case e@EmptyExpr() =>
        e.id = id
        mapToAST += id -> e
        e

      case func@FunctionExpr(name, formals, body) =>
        val f = FunctionExpr(name.map(decorateName(_)),
          formals.map(decorateName(_)),
          decorateStatement(body))
        f.id = id
        mapToAST += id -> f
        f.sourceCode = func.sourceCode
        f.prototypeID = freshID
        f.protoID = freshID
        f.constructorID = freshID
        f

      case x@VarRef(_) =>
        x.id = id
        mapToAST += id -> x
        x

      case t@ThisRef() =>
        t.id = id
        mapToAST += id -> t
        t

      case DotRef(obj, prop) =>
        val dot = DotRef(decorateExpression(obj), prop)
        dot.id = id
        mapToAST += id -> dot
        dot

      case BracketRef(obj, prop) =>
        val bracket = BracketRef(decorateExpression(obj), decorateExpression(prop))
        bracket.id = id
        mapToAST += id -> bracket
        bracket

      case MethodCall(rec, method, args) =>
        val call = MethodCall(decorateExpression(rec),
          decorateExpression(method),
          args.map(decorateExpression(_)))
        call.id = id
        mapToAST += id -> call
        call

      case FuncCall(func, args) =>
        val call = FuncCall(decorateExpression(func),
          args.map(decorateExpression(_)))
        call.id = id
        mapToAST += id -> call
        call

      case NewCall(cons, args) =>
        val call = NewCall(decorateExpression(cons),
          args.map(decorateExpression(_)))

        call.id = id
        call.protoID = freshID
        call.constructorID = freshID
        mapToAST += id -> call
        call

      case AssignExpr(op, lv, expr) =>
        val assign = AssignExpr(op, decorateLValue(lv), decorateExpression(expr))
        assign.id = id
        mapToAST += id -> assign
        assign

      case ObjectLit(o) =>
        val obj = ObjectLit(o.map{
          case ObjectPair(p, expr) =>
            val op = ObjectPair(decorateProperty(p), decorateExpression(expr))
            op.id = freshID
            op
        })
        obj.id = id
        obj.protoID = freshID
        obj.constructorID = freshID
        mapToAST += id -> obj
        obj

      case ArrayLit(vs) =>
        val arr = ArrayLit(vs.map(decorateExpression(_)))
        arr.id = id
        arr.protoID = freshID
        arr.constructorID = freshID
        mapToAST += id -> arr
        arr

      case UnaryAssignExpr(op, lv) =>
        val unary = UnaryAssignExpr(op, decorateLValue(lv))
        unary.id = id
        mapToAST += id -> unary
        unary

      case PrefixExpr(op, expr) =>
        val pre = PrefixExpr(op, decorateExpression(expr))
        pre.id = id
        mapToAST += id -> pre
        pre

      case InfixExpr(op, e1, e2) =>
        val in = InfixExpr(op, decorateExpression(e1), decorateExpression(e2))
        in.id = id
        mapToAST += id -> in
        in

      case CondExpr(e1, e2, e3) =>
        val cond = CondExpr(decorateExpression(e1),
          decorateExpression(e2),
          decorateExpression(e3))
        cond.id = id
        mapToAST += id -> cond
        cond

      case ListExpr(lst) =>
        val l = ListExpr(lst.map(decorateExpression(_)))
        l.id = id
        mapToAST += id -> l
        l

      case expr =>
        expr.id = id
        mapToAST += id -> expr
        expr

    }
  }

  def decorateForInit(init : ForInit) : ForInit = {
    init match {
      case n@NoneInit() => n
      case VarListInit(lst) =>
        val id = freshID
        val varLst = VarListInit(decorateStatement(lst).asInstanceOf[VarDeclListStmt])
        varLst.id = id
        mapToAST += id -> varLst
        varLst
      case VarInit(in) =>
        val id = freshID
        val varInit = VarInit(decorateStatement(in).asInstanceOf[VarDeclStmt])
        varInit.id = id
        mapToAST += id -> varInit
        varInit
      case ExprInit(expr) =>
        val id = freshID
        val exprInit = ExprInit(decorateExpression(expr))
        exprInit.id = id
        exprInit
    }
  }

  def decorateForInInit(init : ForInInit) : ForInInit = {
    val id = freshID
    init match {
      case f@ForInVarDecl(name) =>
        val idx = freshID
        f.name.id = idx
        mapToAST += idx -> f.name
        f.id = id
        mapToAST += id -> f
        f
      case ForInLValue(lvalue) =>
        val lInit = ForInLValue(decorateLValue(lvalue))
        lInit.id = id
        mapToAST += id -> lInit
        lInit
    }
  }

  def decorateLValue(lvalue : LValue) : LValue = {
    val id = freshID
    lvalue match {
      case x@LVarRef(_) =>
        x.id = id
        mapToAST += id -> x
        x

      case LDot(obj, pro) =>
        val ldot = LDot(decorateExpression(obj), pro)
        ldot.id = id
        mapToAST += id -> ldot
        ldot

      case LBracket(obj, pro) =>
        val lb = LBracket(decorateExpression(obj), decorateExpression(pro))
        lb.id = id
        mapToAST += id -> lb
        lb
    }
  }

  def decorateName(name : IntroduceVar) : IntroduceVar = {
    val id = freshID
    name.id = id
    mapToAST += id -> name
    name
  }

  def decorateProperty(prop : Property) : Property = {
    val id = freshID
    prop.id = id
    mapToAST += id -> prop
    prop
  }
}
