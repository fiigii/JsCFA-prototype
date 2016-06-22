import JSSemantics._

/**
  * Created by Fei Peng on 3/30/16.
  */

object AAM {
  import JSSemantics._
  import JSBuiltIn._
  import AAMHelper._

  case class StackAddress(e: AbstractSyntaxTree.Label, env: AbstractSyntaxTree.Label)

  case class GlobalFrame(returnPoint: JSReference, oldStack: LocalStack, oldEnv: Environment, a: StackAddress) {
    override def toString: String = {
      "GlobalFrame:\n      Return Point:" + returnPoint + "\n      oldStack: " + oldStack +
        "\n      oldEnv: " + oldEnv + "\n      next: " + a
    }
  }

  type Environment = Map[String, JSReference]
  type AbstractValue = Set[JSValue]
  type AbstractFrame = Set[GlobalFrame]
  type Stack = scala.collection.mutable.Map[StackAddress, AbstractFrame]
  type LocalStack = List[Continuation]
  type Store = scala.collection.mutable.Map[JSReference, AbstractValue]
  type Disk = scala.collection.mutable.Map[JSReference, AbstractValue]


  case class State(e: AbstractSyntaxTree, env: Environment, localStack: LocalStack, a: StackAddress, memory: Memory) {
    override def toString = {
      "State :\nControl String :" + e.id + " " + e + "\nEnv :" + env + "\nLocal Stack:" +
        localStack + "\na:" + a + "\nOld Stack :" + showOldStack(a)// + "\nStore" + memory.show
    }

    private def showOldStack(a: StackAddress): String = {
      val oldStack = memory.stack(a)
      val str = new StringBuilder("" + oldStack.size)
      oldStack.foreach(x => str.append("\n" + x))
      str.toString
    }
  }

  sealed abstract class AAMException(message: String) extends
    RuntimeException("Abstracting Abstract Machine Exception: " + message)

  class UnDecledVar(name: String) extends AAMException("UnDecledVar: " + name)

  class TypeError extends AAMException("Type Error")

  private val emptyEnv = builtInEnv
  private val emptyLocalStack = Nil

  val initMemory = setBuiltIn()

  val disk: Disk = scala.collection.mutable.Map.empty[JSReference, AbstractValue]

  def analyze(program: Statement): Disk = {
    val initState = inject(program)
    var todo = List(initState)
    val seen = scala.collection.mutable.Set.empty[State]
    seen.add(initState)


    var i = 0

    while (todo.nonEmpty) {
      val currentState = todo.head
      todo = todo.tail

      //println("\nState : " + i)
      i += 1
      //println(currentState + "\n")
      /*
      println("\nStack :")
      for ((p,f) <- currentState.memory.stack) {
        println(p + " -> ")
        println("    " + f)
      }
      */
      val nextStates = transitEvaluation(currentState)

      for (next <- nextStates) {
        if (!seen.contains(next)) {
          seen.add(next)
          todo = next :: todo
        } else {
          //println("\n\nSEEN: " + next)
        }
      }
    }
    println("TOTAL : " + i)
    disk
  }

  def inject(program: Statement): State = State(program, emptyEnv, emptyLocalStack, startAddress, initMemory)

  def transitEvaluation(state: State): Set[State] = state match {
    //Application
    case State(complete, _, _, _, _) if isComplete(complete) => transitApplication(state)
    //Evaluation
    //Statement

    case State(Script(Nil), env, localStack, a, memory) => Set(State(Halt, env, localStack, a, memory))
    case State(Script(stmt :: ss), env, localStack, a, memory) =>
      val k = KScript(ss)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(stmt, env, newStack, a, memory))

    case State(BlockStmt(stmt :: Nil), env, localStack, a, memory) => Set(State(stmt, env, localStack, a, memory))
    case State(BlockStmt(stmt :: ss), env, localStack, a, memory) =>
      val k = KBlockStmt(ss)
      //k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(stmt, env, newStack, a, memory))

    case State(VarDeclListStmt(stmt :: Nil), env, localStack, a, memory) => Set(State(stmt, env, localStack, a, memory))
    case State(VarDeclListStmt(stmt :: ss), env, localStack, a, memory) =>
      val k = KVarDeclListStmt(ss)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(stmt, env, newStack, a, memory))

    case State(ExprStmt(e), env, localStack, a, memory) => Set(State(e, env, localStack, a, memory))

    case State(VarDeclStmt(name, e), env, localStack, a, memory) =>
      val k = KVarDeclStmt(name)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      val newAddress = alloc(name)
      val newEnv = updateEnv(env, name.str, newAddress)
      Set(State(e, newEnv, newStack, a, memory))


    case State(f@FunctionDecl(x, body), env, localStack, a, memory) =>
      val k = KFunctionDeclBody(x)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(body, env, newStack, a, memory))

    case State(ReturnStmt(e), env, localStack, a, memory) =>
      val k = KReturn()
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(e, env, newStack, a, memory))

    case State(IfStmt(cond, t, e), env, localStack, a, memory) =>
      val k = KIfCond(t, e)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(cond, env, newStack, a, memory))

    case State(DoWhileStmt(cond, body), env, localStack, a, memory) =>
      val k = KDoWhileBody(cond, body)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(body, env, newStack, a, memory))

    case State(WhileStmt(cond, body), env, localStack, a, memory) =>
      val k = KWhileE(body)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(cond, env, newStack, a, memory))

    case State(SwitchStmt(cond, cases, defaultCase), env, localStack, a, memory) =>
      val k = KSwitchE(cases, defaultCase)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(cond, env, newStack, a, memory))

    case State(ForStmt(init, cond, increment, body), env, localStack, a, memory) =>
      val k = KForI(cond, increment, body)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(init, env, newStack, a, memory))

    case State(ForInStmt(init, expr, body), env, localStack, a, memory) =>
      val k = KForInE(init, body)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(expr, env, newStack, a, memory))

    // Expression
    case State(DotRef(obj, prop), env, localStack, a, memory) =>
      val k = KDotRef(prop)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(obj, env, newStack, a, memory))

    case State(BracketRef(obj, prop), env, localStack, a, memory) =>
      val k = KBracketO(prop)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(obj, env, newStack, a, memory))

    case State(MethodCall(receiver, method, args), env, localStack, a, memory) =>
      val k = KMethodCallR(method, args)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(receiver, env, newStack, a, memory))

    case State(FuncCall(func, args), env, localStack, a, memory) =>
      val k = KFuncCallF(args)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(func, env, newStack, a, memory))

    case State(NewCall(constructor, args), env, localStack, a, memory) =>
      val k = KNewCallF(args)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(constructor, env, newStack, a, memory))

    case State(AssignExpr(op, lv, expr), env, localStack, a, memory) =>
      val k = KAssignR(op, lv)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(expr, env, newStack, a, memory))

    case State(ObjectLit(Nil), env, localStack, a, memory) =>
      val k = KObjectComplete(scala.collection.mutable.Map())
      k.generateFrom(state.e)
      Set(State(k, env, localStack, a, memory))

    case State(ObjectLit(p :: ps), env, localStack, a, memory) =>
      val k = KObjectLit(scala.collection.mutable.Map.empty[JSString, JSValue], ps)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(p, env, newStack, a, memory))

    case State(ObjectPair(p, e), env, localStack, a, memory) =>
      val propName = ToString(p)
      val k = KObjectPairE(propName)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(e, env, newStack, a, memory))

    case State(ArrayLit(Nil), env, localStack, a, memory) =>
      val k = KArrayComplete(Nil)
      k.generateFrom(state.e)
      Set(State(k, env, localStack, a, memory))

    case State(ArrayLit(ele :: es), env, localStack, a, memory) =>
      val k = KArrayLit(Nil, es)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(ele, env, newStack, a, memory))

    case State(UnaryAssignExpr(op, lv), env, localStack, a, memory) =>
      val k = KUnaryAssign(op)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(lv, env, newStack, a, memory))

    case State(PrefixExpr(op, expr), env, localStack, a, memory) =>
      val k = KPrefix(op)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(expr, env, newStack, a, memory))

    case State(InfixExpr(op, e1, e2), env, localStack, a, memory) =>
      val k = KInfixL(op, e2)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(e1, env, newStack, a, memory))

    case State(CondExpr(cond, thenPart, elsePart), env, localStack, a, memory) =>
      val k = KCondC(thenPart, elsePart)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(cond, env, newStack, a, memory))

    case State(ListExpr(fst :: list), env, localStack, a, memory) =>
      val k = KList(list)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(fst, env, newStack, a, memory))

    //LValue
    case State(LDot(obj, field), env, localStack, a, memory) =>
      val k = KLDotRef(field)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(obj, env, newStack, a, memory))

    case State(LBracket(obj, computeField), env, localStack, a, memory) =>
      val k = KLBracketO(computeField)
      k.generateFrom(state.e)
      val newStack = pushLocalStack(localStack, k)
      Set(State(obj, env, newStack, a, memory))

    //ForInit
    case State(NoneInit(), env, localStack, a, memory) =>
      Set(State(cachedUndefined, env, localStack, a, memory))

    case State(VarListInit(vars), env, localStack, a, memory) =>
      Set(State(vars, env, localStack, a, memory))

    case State(VarInit(varDecl), env, localStack, a, memory) =>
      Set(State(varDecl, env, localStack, a, memory))

    case State(ExprInit(e), env, localStack, a, memory) =>
      Set(State(e, env, localStack, a, memory))


    //Continuation
    case State(v, env, localStack, a, memory) if isJSValue(v) =>
      if (localStack.nonEmpty) {
        val cont = topOfLocalStack(localStack)
        val newStack = popLocalStack(localStack)
        transitContinuation(cont, v.asInstanceOf[JSValue], env, newStack, a, memory)
      } else if (a != startAddress) {
        for {
          GlobalFrame(returnPoint, oldStack, savedEnv, newGlobalAddress) <- memory.globalFrames(a)
        } yield {
          if(oldStack.isEmpty || !oldStack.head.isInstanceOf[KUseValue]) {
            val newMemory = memory.copy(state)
            newMemory.putValue(returnPoint, JSUndefined)
            State(returnPoint, savedEnv, oldStack, newGlobalAddress, newMemory)
          } else {
            State(returnPoint, savedEnv, oldStack, newGlobalAddress, memory)
          }
        }
      } else {
        Set(State(Halt, env, localStack, a, memory))
      }

    case State(Halt, _ ,_ ,_, _) => Set()

    case _ => throw new RuntimeException("Mismatch " + state + "\nin Evaluation")

  }

  def transitContinuation(cont: Continuation, value: JSValue, env: Environment, localStack: LocalStack, a: StackAddress, memory: Memory): Set[State] = cont match {
    case KScript(Nil) => Set(State(Halt, env, localStack, a, memory))
    case KScript(s :: ss) =>
      val k = KScript(ss)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(s, env, newStack, a, memory))

    case KBlockStmt(s :: Nil) => Set(State(s, env, localStack, a, memory))
    case KBlockStmt(s :: ss) =>
      val k = KBlockStmt(ss)
      val newStack = pushLocalStack(localStack, k)
      Set(State(s, env, newStack, a, memory))

    case KVarDeclListStmt(s :: Nil) => Set(State(s, env, localStack, a, memory))
    case KVarDeclListStmt(s :: ss) =>
      val k = KVarDeclListStmt(ss)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(s, env, newStack, a, memory))

    case KVarDeclStmt(name) =>
      val k = KVarDeclStmtComplete(name, value)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KFunctionDeclBody(name) =>
      val k = KFunctionDeclComplete(name, value)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KReturn() =>
      val k = KReturnComplete(value)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KIfCond(t, e) =>
      val k = KIfComplete(value, t, e)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KCondC(t, e) =>
      val k = KCondComplete(value, t, e)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KWhileE(body) =>
      val k = KWhileComplete(value, body)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KDoWhileBody(cond, body) =>
      val k = KDoWhileE(cond, body)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(cond, env, newStack, a, memory))

    case KDoWhileE(cond, body) =>
      val k = KDoWhileComplete(value, cond, body)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KDotRef(p) =>
      val k = KDotRefComplete(value, p)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KBracketO(prop) =>
      val k = KBracketP(value)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(prop, env, newStack, a, memory))

    case KBracketP(obj) =>
      val k = KBracketComplete(obj, value)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KMethodCallR(method, args) =>
      val k = KMethodCallF(value, args)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(method, env, newStack, a, memory))

    case KMethodCallF(receiver, Nil) =>
      val k = KMethodCallA(receiver, value, Nil, Nil)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(cachedUndefined, env, newStack, a, memory))

    case KMethodCallF(receiver, arg :: args) =>
      val k = KMethodCallA(receiver, value, Nil, args)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(arg, env, newStack, a, memory))

    case KMethodCallA(receiver, method, before, Nil) =>
      val k = KMethodCallComplete(receiver, method, before ++ List(value))
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KMethodCallA(receiver, method, before, arg :: args) =>
      val k = KMethodCallA(receiver, method, before ++ List(value), args)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(arg, env, newStack, a, memory))

    case KFuncCallF(Nil) =>
      val k = KFuncCallA(value, Nil, Nil)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(cachedUndefined, env, newStack, a, memory))

    case KFuncCallF(arg :: args) =>
      val k = KFuncCallA(value, Nil, args)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(arg, env, newStack, a, memory))

    case KFuncCallA(func, before, Nil) =>
      val k = KFuncCallComplete(func, before ++ List(value))
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KFuncCallA(func, before, arg :: args) =>
      val k = KFuncCallA(func, before ++ List(value), args)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(arg, env, newStack, a, memory))

    case KNewCallF(Nil) =>
      val k = KNewCallA(value, Nil, Nil)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(cachedUndefined, env, newStack, a, memory))

    case KNewCallF(arg :: args) =>
      val k = KNewCallA(value, Nil, args)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(arg, env, newStack, a, memory))

    case KNewCallA(func, before, Nil) =>
      val k = KNewCallComplete(func, before ++ List(value))
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KNewCallA(func, before, arg :: args) =>
      val k = KNewCallA(func, before ++ List(value), args)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(arg, env, newStack, a, memory))

    case KAssignR(op, lv) =>
      val k = KAssignL(op, value)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(lv, env, newStack, a, memory))

    case KAssignL(op, rv) =>
      val k = KAssignExprComplete(op, value, rv)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KObjectPairE(name) =>
      val k = KObjectPairPack(name, value)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KObjectLit(before, Nil) =>
      value match {
        case KObjectPairPack(key, v) =>
          before(key) = v
        case _ => throw new RuntimeException("ObjectLit wants a bjectPairPack, not " + value)
      }
      val k = KObjectComplete(before)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KObjectLit(before, p :: ps) =>
      value match {
        case KObjectPairPack(key, v) =>
          before(key) = v
          val k = KObjectLit(before, ps)
          k.generateFrom(cont)
          val newStack = pushLocalStack(localStack, k)
          Set(State(p, env, newStack, a, memory))

        case _ => throw new RuntimeException("Value :" + value + "is not a KObjectPairPack.")
      }

    case KArrayLit(before, Nil) =>
      val k = KArrayComplete(before :+ value)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KArrayLit(before, p :: ps) =>
      val k = KArrayLit(before :+ value, ps)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(p, env, newStack, a, memory))

    case KInfixL(op, e2) =>
      val k = KInfixR(op, value)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(e2, env, newStack, a, memory))

    case KInfixR(op, e1) =>
      val k = KInfixExprComplete(op, e1, value)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KPrefix(op) =>
      val k = KPrefixExprComplete(op, value)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KUnaryAssign(op) =>
      val k = KUnaryAssignComplete(op, value)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))


    case KList(e :: es) =>
      val k = KList(es)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(e, env, newStack, a, memory))


    case KLDotRef(p) =>
      val k = KLDotRefComplete(value, p)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KLBracketO(prop) =>
      val k = KLBracketP(value)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(prop, env, newStack, a, memory))

    case KLBracketP(obj) =>
      val k = KLBracketComplete(obj, value)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    //ForInInit

    case KForInE(init, body) =>
      val k = KForInI(value, body)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(init, env, newStack, a, memory))

    case KForInI(expr, body) =>
      val k = KForInComplete(value, expr, body)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KForI(Some(cond), increment, body) =>
      val k = KForC(increment, body)
      k.generateFrom(cont)
      val newStack = pushLocalStack(localStack, k)
      Set(State(cond, env, newStack, a, memory))

    case KForI(None, increment, body) =>
      val k = KForCompleteTrue(increment, body)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    case KForC(increment, body) =>
      val k = KForComplete(value, increment, body)
      k.generateFrom(cont)
      Set(State(k, env, localStack, a, memory))

    //Special
    case KUseValue(v) => Set(State(v, env, localStack, a, memory))

    case complete if isComplete(complete) => transitApplication(State(complete, env, localStack, a, memory))

    case _ => throw new RuntimeException("Unmatched continuation " + cont)
  }

  def transitApplication(state: State): Set[State] = state match {
    //Statements
    case State(EmptyStmt(), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      val newAddress = alloc(state.e)
      newMemory.putValue(newAddress, JSUndefined)
      Set(State(newAddress, env, localStack, a, newMemory))

    case State(EmptyExpr(), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      val newAddress = alloc(state.e)
      newMemory.putValue(newAddress, JSUndefined)
      Set(State(newAddress, env, localStack, a, newMemory))

    case State(KReturnComplete(v), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      for {
        GlobalFrame(returnPoint, oldStack, savedEnv, newGlobalAddress) <- newMemory.globalFrames(a)
      } yield {
        if(oldStack.isEmpty || !oldStack.head.isInstanceOf[KUseValue]) {
          for(vs <- newMemory.getValue(v)) {
            newMemory.putValue(returnPoint, vs)
          }
        }
        State(returnPoint, savedEnv, oldStack, newGlobalAddress, newMemory)
      }

    case State(KIfComplete(cond, t, e), env, localStack, a, memory) =>
      for {
        obj <- memory.getValue(cond)
        boolValue = ToBoolean(obj)
        res <- boolValue match {
          case JSBoolean(ConstantBoolean(true)) => Set(State(t, env, localStack, a, memory))
          case JSBoolean(ConstantBoolean(false)) => Set(State(e, env, localStack, a, memory))
          case JSBoolean(VariableBoolean) => Set(State(t, env, localStack, a, memory), State(e, env, localStack, a, memory))
          case _ => throw new TypeError()
        }
      } yield res

    case State(KCondComplete(cond, t, e), env, localStack, a, memory) =>
      for {
        bValue <- memory.getValue(cond)
        boolValue = ToBoolean(bValue)
        res <- boolValue match {
          case JSBoolean(ConstantBoolean(true)) => Set(State(t, env, localStack, a, memory))
          case JSBoolean(ConstantBoolean(false)) => Set(State(e, env, localStack, a, memory))
          case JSBoolean(VariableBoolean) => Set(State(t, env, localStack, a, memory), State(e, env, localStack, a, memory))
        }
      } yield res

    case State(KWhileComplete(cond, body), env, localStack, a, memory) =>
      for {
        condValue <- memory.getValue(cond)
        bool = ToBoolean(condValue)
      } yield bool match {
        case JSBoolean(ConstantBoolean(false)) => State(cachedUndefined, env, localStack, a, memory)
        case _ =>
          body match {
            case BlockStmt(stmts) => State(BlockStmt(stmts ++ stmts), env, localStack, a, memory)
            case _ => State(BlockStmt(List(body, body)), env, localStack, a, memory)
          }
        /*
      case JSBoolean(VariableBoolean) =>
        val newBody = if(body.isInstanceOf[BlockStmt]) {
          val b = body.asInstanceOf[BlockStmt]
          BlockStmt(b.stmts ++ b.stmts)
        } else {
          BlockStmt(List(body, body))
        }
        Set(State(newBody, env, localStack, a, memory), State(cachedUndefined, env, localStack, a, memory))*/
      }

    case State(KDoWhileComplete(cond, e, body), env, localStack, a, memory) =>
      for {
        bValue <- memory.getValue(cond)
        boolValue = ToBoolean(bValue)
        res = boolValue match {
          case JSBoolean(ConstantBoolean(false)) =>  State(cachedUndefined, env, localStack, a, memory)
          case _ =>
            body match {
              case BlockStmt(stmts) => State(BlockStmt(stmts :+ ExprStmt(e)), env, localStack, a, memory)
              case _ => State(BlockStmt(List(body, ExprStmt(e))), env, localStack, a, memory)
            }

        }
      } yield res

    case State(KForComplete(cond, increment,body), env, localStack, a, memory) =>
      for {
        condValue <- memory.getValue(cond)
        bool = ToBoolean(condValue)
      } yield bool match {
        case JSBoolean(ConstantBoolean(false)) => State(cachedUndefined, env, localStack, a, memory)
        case _ =>
          increment match {
            case None =>
              body match {
                case BlockStmt(stmts) => State(BlockStmt(stmts ++ stmts), env, localStack, a, memory)
                case _ => State(BlockStmt(List(body, body)), env, localStack, a, memory)
              }
            case Some(inc) =>
              body match {
                case BlockStmt(stmts) => State(BlockStmt(stmts ++ List(ExprStmt(inc)) ++ stmts ++ List(ExprStmt(inc))), env, localStack, a, memory)
                case _ => State(BlockStmt(List(body, ExprStmt(inc), body, ExprStmt(inc))), env, localStack, a, memory)
              }
          }
      }

    case State(KForCompleteTrue(increment, body), env, localStack, a, memory) =>
      val s = increment match {
        case None =>
          body match {
            case BlockStmt(stmts) => State(BlockStmt(stmts ++ stmts), env, localStack, a, memory)
            case _ => State(BlockStmt(List(body, body)), env, localStack, a, memory)
          }
        case Some(inc) =>
          body match {
            case BlockStmt(stmts) => State(BlockStmt(stmts ++ List(ExprStmt(inc)) ++ stmts ++ List(ExprStmt(inc))), env, localStack, a, memory)
            case _ => State(BlockStmt(List(body, ExprStmt(inc), body, ExprStmt(inc))), env, localStack, a, memory)
          }
      }
      Set(s)

    //Expressions
    case State(VarRef(x), env, localStack, a, memory) =>
      val xRef = lookup(env, x)
      val newMemory = memory.copy(state)
      for(vs <- newMemory.getValue(xRef)){
        newMemory.putValue(JSReference(state.e.id), vs)
      }
      Set(State(JSReference(state.e.id), env, localStack, a, newMemory))

    case State(ThisRef(), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      val thisRef = lookup(env, "this")
      //val newAddress = alloc(state.e)
      //newMemory.getValue(thisRef).foreach(newMemory.putValue(newAddress, _))
      Set(State(thisRef, env, localStack, a, newMemory))

    case State(f@FunctionExpr(name, ps, body), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      /*
      val closureEnv = name match {
        case None => env
        case Some(i@IntroduceVar(functionName)) => updateEnv(env, functionName, alloc(i))
      } */
      val functionObject = createFunctionObject(f, env, newMemory)
      val value = newMemory.save(functionObject)
      Set(State(value, env, localStack, a, newMemory))

    case State(NullLit(), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      val newAddress = alloc(state.e)
      newMemory.putValue(newAddress, JSNull)
      Set(State(newAddress, env, localStack, a, newMemory))

    case State(NumberLit(num), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      val number = JSNumber(ConstantNumber(num))
      number.generateFrom(state.e)
      val value = newMemory.save(number)
      Set(State(value, env, localStack, a, newMemory))

    case State(BoolLit(b), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      val bool = JSBoolean(ConstantBoolean(b))
      bool.generateFrom(state.e)
      val value = newMemory.save(bool)
      Set(State(value, env, localStack, a, newMemory))

    case State(StringLit(s), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      val str = JSString(ConstantString(s))
      str.generateFrom(state.e)
      val value = newMemory.save(str)
      Set(State(value, env, localStack, a, newMemory))

    case State(r@RegExp(s, g, c), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      val regexp = createRegExpObject(r, newMemory)
      regexp.generateFrom(state.e)
      val value = newMemory.save(regexp)
      Set(State(value, env, localStack, a, newMemory))

    case State(LVarRef(x), env, localStack, a, memory) =>
      val xRef = lookup(env, x)
      Set(State(xRef, env, localStack, a, memory))

    case State(KDotRefComplete(objRef, prop), env, localStack, a, memory) =>
      for {
        obj <- memory.getValue(objRef)
        if canToObject(obj)
        o = ToObject(obj)
        value <- o.lookup(JSString(ConstantString(prop)), memory)
      } yield State(value, env, localStack, a, memory)

    case State(KBracketComplete(objRef, propRef), env, localStack, a, memory) =>
      for {
        obj <- memory.getValue(objRef)
        if canToObject(obj)
        o = ToObject(obj)
        prop <- memory.getValue(propRef)
        pstr = ToString(prop)
        value <- o.lookup(pstr, memory)
      } yield State(value, env, localStack, a, memory)

    case State(KLDotRefComplete(objRef, prop), env, localStack, a, memory) =>
      for {
        obj <- memory.getValue(objRef)
        if canToObject(obj)
        objValue = ToObject(obj)
        values = objValue.lookupOwn(JSString(ConstantString(prop)))
      } yield {
        if (values == cachedUndefined) {
          val newMemory = memory.copy(state)
          val newAddress = alloc(state.e)
          objValue.addField(JSString(ConstantString(prop)), newAddress, objRef, newMemory)
          State(newAddress, env, localStack, a, newMemory)
        } else {
          State(values, env, localStack, a, memory)
        }
      }

    case State(KLBracketComplete(objRef, propRef), env, localStack, a, memory) =>
      for {
        obj <- memory.getValue(objRef)
        if canToObject(obj)
        objValue = ToObject(obj)
        prop <- memory.getValue(propRef)
        pstr = ToString(prop)
        values = objValue.lookupOwn(pstr)
      } yield if (values == cachedUndefined) {
        val newMemory = memory.copy(state)
        val newAddress = alloc(state.e)
        objValue.addField(pstr, newAddress, objRef, newMemory)
        State(newAddress, env, localStack, a, newMemory)
      } else {
        State(values, env, localStack, a, memory)
      }

    case State(KMethodCallComplete(receiver, method, args), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      for {
        recs <- newMemory.getValue(receiver)
        if canToObject(recs)
        rec = ToObject(recs)
        metName <- newMemory.getValue(method)
        if metName.isInstanceOf[JSString]
        metRef <- rec.lookup(metName.asInstanceOf[JSString], newMemory)
        met <- newMemory.getValue(metRef)
        if isCallable(met)
        JSClosure(func@FunctionExpr(name, ps, funcBody), savedEnv) = met.asInstanceOf[JSObject].code

      } yield {
        if(isCallBuiltIn(met.asInstanceOf[JSObject])) {
          methodBuiltInCall(rec, met.asInstanceOf[JSObject], args, state)
        } else {
          val psAddress = ps.map(alloc(_))
          //val thisAddress = alloc(func.thisID)
          var newEnvPart = ("this" -> receiver.asInstanceOf[JSReference]) :: ps.map(x => x.str).zip(psAddress)
          name match {
            case Some(x) => newEnvPart = (x.str -> alloc(x)) :: newEnvPart
            case None =>
          }
          val newEnv = savedEnv ++ Map(newEnvPart: _*)
          //putValue(thisAddress, rec)
          psAddress.zip(args).foreach[Unit](
            (p: (JSReference, JSValue)) => newMemory.getValue(p._2).foreach(newMemory.putValue(p._1, _)))
          val nextAddress = allocStackAddress(state, funcBody, newEnv)
          newMemory.pushGlobalStack(nextAddress, GlobalFrame(alloc(state.e), localStack, env, a))
          State(funcBody, newEnv, emptyLocalStack, nextAddress, newMemory)
        }
      }

    case State(KFuncCallComplete(funcRef, args), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      for {
        //rec <- memory.getValue(biGlobalObjectRef)
        f <- newMemory.getValue(funcRef)
        if isCallable(f)
        JSClosure(func@FunctionExpr(name, ps, funcBody), savedEnv) = f.asInstanceOf[JSObject].code
      } yield {
        if(isCallBuiltIn(f.asInstanceOf[JSObject])) {
          funcBuiltInCall(f.asInstanceOf[JSObject], args, state)
        } else {
          val psAddress = ps.map(alloc(_))
          val thisAddress = biGlobalObjectRef
          var newEnvPart = ("this" -> thisAddress) :: ps.map(x => x.str).zip(psAddress)
          name match {
            case Some(x) => newEnvPart = (x.str -> alloc(x)) :: newEnvPart
            case None =>
          }
          val newEnv = savedEnv ++ Map(newEnvPart: _*)
          for(p <- psAddress.zip(args)) {
            for(vs <- newMemory.getValue(p._2)) {
              newMemory.putValue(p._1, vs)
            }
          }
          val nextAddress = allocStackAddress(state, funcBody, newEnv)
          newMemory.pushGlobalStack(nextAddress, GlobalFrame(alloc(state.e), localStack, env, a))
          State(funcBody, newEnv, emptyLocalStack, nextAddress, newMemory)
        }
      }

    case State(kn@KNewCallComplete(constructor, args), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      for {
        f <- newMemory.getValue(constructor)
        if isCallable(f)
        if canToObject(f)
        cons = ToObject(f)
        prototype <- cons.lookup(JSString(ConstantString("prototype")), newMemory)
      } yield {
        //create __proto__ object
        val rec = newMemory.createEmptyObject(prototype, constructor.asInstanceOf[JSReference], kn)
        rec.generateFrom(kn)
        val JSClosure(FunctionExpr(name, ps, funcBody), savedEnv) = cons.code
        //alloc parameters
        val psAddress = ps.map(alloc(_))

        val thisAddress = alloc(kn)

        //update environment
        var newEnvPart = ("this" -> thisAddress) :: ps.map(x => x.str).zip(psAddress)
        name match {
          case Some(x) => newEnvPart = (x.str -> alloc(x)) :: newEnvPart
          case None =>
        }
        val newEnv = savedEnv ++ Map(newEnvPart: _*)

        //pass arguments
        newMemory.putValue(thisAddress, rec)
        psAddress.zip(args).foreach[Unit]((p: (JSReference, JSValue)) => newMemory.getValue(p._2).foreach(newMemory.putValue(p._1, _)))

        //return "this"
        val newLocalStack = KUseValue(thisAddress) :: localStack

        //push call stack
        val nextAddress = allocStackAddress(state, funcBody, newEnv)
        newMemory.pushGlobalStack(nextAddress, GlobalFrame(alloc(state.e), newLocalStack, env, a))
        State(funcBody, newEnv, emptyLocalStack, nextAddress, newMemory)
      }

    case State(KAssignExprComplete(op, lv, rv), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      for(value <- newMemory.getValue(rv)) {
        newMemory.putValue(lv.asInstanceOf[JSReference], value)
      }
      Set(State(rv, env, localStack, a, newMemory))

    case State(ko@KObjectComplete(pairs), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      val content = pairs.map {
        case (name, v: JSReference) => (name, v)
        case wrong => throw new RuntimeException("Object Value :" + wrong + "is not Reference Based.")
      }
      val obj = newMemory.createEmptyObject(biObjectProtoRef, biObjectRef,ko,
        collection.mutable.Map(content.toSeq: _*))
      obj.generateFrom(state.e)
      val objRef = newMemory.save(obj)
      Set(State(objRef, env, localStack, a, newMemory))

    case State(ka@KArrayComplete(elements), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      val arrayObj = createArrayObject(ka, elements, newMemory)
      arrayObj.generateFrom(state.e)
      val arrayObjRef = newMemory.save(arrayObj)
      Set(State(arrayObjRef, env, localStack, a, newMemory))

    case State(KUnaryAssignComplete(op, lv), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      //TODO make this state deterministic
      for {
        v <- unaryAssignFunction(op, lv, newMemory)
      } yield {

        val newAddress = alloc(state.e)
        newMemory.putValue(newAddress,v)
        State(newAddress, env, localStack, a, newMemory)
      }

    case State(KPrefixExprComplete(op, rv), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      for {
        v <- newMemory.getValue(rv)
      } yield {
        val res = prefixFunc(op, v)
        res.generateFrom(state.e)
        val newAddress = newMemory.save(res)
        State(newAddress, env, localStack, a, newMemory)
      }

    case State(KInfixExprComplete(op, rv1, rv2), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      for {
        v1 <- newMemory.getValue(rv1)
        v2 <- newMemory.getValue(rv2)
      } yield {
        val res = infixFunc(op, v1, v2, newMemory)
        res.generateFrom(state.e)
        val address = newMemory.save(res)
        State(address, env, localStack, a, newMemory)
      }

    case State(KList(e :: Nil), env, localStack, a, memory) => Set(State(e, env, localStack, a, memory))

    case State(KVarDeclStmtComplete(name, v), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      val newAddress = alloc(name)
      newMemory.getValue(v).foreach((value: JSValue) => newMemory.putValue(newAddress, value))
      Set(State(cachedUndefined, env, localStack, a, newMemory))




    case State(KFunctionDeclComplete(name, func), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      val newAddress = alloc(name)
      val newEnv = updateEnv(env, name.str, newAddress)
      newMemory.getValue(func).foreach((value: JSValue) => newMemory.putValue(newAddress, value))
      Set(State(cachedUndefined, newEnv, localStack, a, newMemory))

    //case State(KObjectPairComplete())

    case State(KForInComplete(init, expr, body), env, localStack, a, memory) =>
      val newMemory = memory.copy(state)
      for {
        value <- newMemory.getValue(expr)
        if init.isInstanceOf[JSReference]
      } yield {
        val keys = ToObject(value).keys(newMemory)
        newMemory.putSetValue(init.asInstanceOf[JSReference], keys.toSet)
        State(body, env, localStack, a, newMemory)
      }

    //ForInInit
    case State(ForInVarDecl(name), env, localStack, a, memory) =>
      val newAddress = alloc(name)
      val newEnv = updateEnv(env, name.str, newAddress)
      Set(State(newAddress, newEnv, localStack, a, memory))

    case State(ForInLValue(lv), env, localStack, a, memory) =>
      Set(State(lv, env, localStack, a, memory))

    case _ => throw new RuntimeException("State : " + state + "is not Complete state.")

  }


  def allocStackAddress(state: State, next: AbstractSyntaxTree, newEnv: Environment): StackAddress =
    StackAddress(next.id, state.e.id)

  def alloc(l: AbstractSyntaxTree.Label) = JSReference(l)

  def alloc(e: AbstractSyntaxTree): JSReference = e match {
    case x@(VarRef(_)) => JSReference(x.referTo.id)
    case x => JSReference(x.id)
  }

  def topOfLocalStack(localStack: LocalStack): Continuation = localStack.head

  def pushLocalStack(localStack: LocalStack, cont: Continuation): LocalStack = cont :: localStack

  def popLocalStack(localStack: LocalStack): LocalStack = localStack.tail

  def updateEnv(oldEnv: Environment, x: String, ref: JSReference): Environment = oldEnv + (x -> ref)

  def lookup(env: Environment, name: String): JSReference = env.get(name) match {
    case None => throw new UnDecledVar(name)
    case Some(v) => v
  }

  def isJSValue(obj: Any): Boolean = obj.isInstanceOf[JSValue]

  def isFinalState(state: State) = state.e == Halt

}
