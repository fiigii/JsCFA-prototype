import AAM.{GlobalFrame, State, LocalStack, Environment, StackAddress}
import JSBuiltIn.{startFrame, startAddress}

/**
  * Created by Fei Peng on 5/3/16.
  */
object GarbageCollector {

  val markSet = collection.mutable.Set.empty[JSReference]
  val markSetK = collection.mutable.Set(startAddress)
  private val seenFrame = collection.mutable.Set(startFrame)

  def startGC(state: State, memory: Memory): Unit = {
    initGC()
    val root = rootSet(state, memory)
    traceFrom(root, memory)
    //sweep(memory)

    val stackRoot = state.a
    traceStack(stackRoot, memory)
    //sweepStack(memory)
  }

  def initGC() : Unit = {
    markSet.clear()
    markSetK.clear()
    markSetK.add(startAddress)
    seenFrame.clear()
    seenFrame.add(startFrame)
  }

  def rootSet(state: State, memory: Memory) : Set[JSReference] = state match {
    case State(e, env, lstack, a, _) =>
      val currSet = rootSet(e) ++ rootSet(env) ++ rootSet(lstack)
      val globalSet = memory.stack(a).flatMap(rootSet(_, memory))
      val root = currSet ++ globalSet
      root
  }

  def rootSet(frame: GlobalFrame, memory: Memory) : Set[JSReference] ={
    if(!seenFrame.contains(frame)) {
      seenFrame.add(frame)
      frame match {
        case GlobalFrame(returnPoint, oldStack, oldEnv, a)  =>
          if (a == startAddress) {
            rootSet(returnPoint) ++ rootSet(oldEnv) ++ rootSet(oldStack)
          } else {
            rootSet(returnPoint) ++ rootSet(oldEnv) ++ rootSet(oldStack) ++ memory.stack(a).flatMap(rootSet(_, memory))
          }
      }
    } else Set()
  }

  def rootSet(lstack: LocalStack) : Set[JSReference] = lstack match {
    case Nil => Set()
    case k :: lastStack =>
      val top = rootSet(k)
      top ++ rootSet(lastStack)
  }

  def rootSet(cont : Continuation) : Set[JSReference] = cont match {
    case KVarDeclStmtComplete(_, v) => rootSet(v)
    case KFunctionDeclComplete(_, v) => rootSet(v)
    case KReturnComplete(v) => rootSet(v)
    //TODO Switch
    case KWhileComplete(v, _) => rootSet(v)
    case KForInI(v, _) => rootSet(v)
    case KForInComplete(v1, v2, _) => rootSet(v1, v2)
    case KDotRefComplete(v, _) => rootSet(v)
    case KBracketP(v) => rootSet(v)
    case KBracketComplete(v1, v2) => rootSet(v1, v2)
    case KMethodCallF(v, _) => rootSet(v)
    case KMethodCallA(receiver, method, before, _) => rootSet(receiver, method) ++ rootSet(before:_*)
    case KMethodCallComplete(receiver, method, args) => rootSet(receiver, method) ++ rootSet(args:_*)
    case KFuncCallA(func, before, _) => rootSet(func) ++ rootSet(before:_*)
    case KFuncCallComplete(func,args) => rootSet(func) ++ rootSet(args:_*)
    case KNewCallA(func, before, _) => rootSet(func) ++ rootSet(before:_*)
    case KNewCallComplete(func,args) => rootSet(func) ++ rootSet(args:_*)
    case KAssignL(_, v) => rootSet(v)
    case KAssignExprComplete(_, lv, expr) => rootSet(lv, expr)
    case KObjectLit(before, _) => rootSet(before.values.toSeq :_*)
    case KObjectComplete(pairs) => rootSet(pairs.values.toSeq :_*)
    case KArrayLit(before, _) => rootSet(before :_*)
    case KArrayComplete(elements) => rootSet(elements : _*)
    case KUnaryAssignComplete(_, v) => rootSet(v)
    case KPrefixExprComplete(_, v) => rootSet(v)
    case KInfixR(_, v) => rootSet(v)
    case KInfixExprComplete(_, v1 ,v2) => rootSet(v1, v2)
    case KUseValue(v) => rootSet(v)
    case KLDotRefComplete(v, _) => rootSet(v)
    case KLBracketP(v) => rootSet(v)
    case KLBracketComplete(v1, v2) => rootSet(v1, v2)

    case _ => Set()
  }

  def rootSet(vs : AbstractSyntaxTree*) : Set[JSReference] = {
    val res = scala.collection.mutable.Set.empty[JSReference]
    for(v <- vs) {
      v match {
        case ref : JSReference => res += ref
        case KObjectPairPack(_, value) => res ++= rootSet(value)
        case k : Continuation if AAMHelper.isComplete(k) => res ++= rootSet(k)
        case _ =>
      }
    }
    res.toSet
  }

  def rootSet(env : Environment) : Set[JSReference] = env.values.toSet

  def traceFrom(root : Set[JSReference], memory: Memory): Unit ={
    root.foreach(ref => {
      trace(ref, memory)
    })
  }

  def trace(ref : JSReference, memory: Memory): Unit ={
    if(!ref.isBuiltIn && memory.store.contains(ref) && !markSet.contains(ref)) {
      markSet.add(ref)

      memory.getValue(ref).foreach {
        case obj@JSObject(content) =>
          content.foreach {
            case (name, value) =>
              trace(value, memory)
          }
          if(obj.code != null && obj.builtIn == null) {
            val env = obj.code.env
            obj.code.function.freeVariables.foreach {
              case freeVar =>
                env.get(freeVar) match {
                  case None =>
                  case Some(freeRef) =>
                    trace(freeRef, memory)
                }
            }
          }
        case other => //markSet.add(ref)
      }
    }
  }

  def sweep(memory: Memory): Unit = {
    memory.store.foreach {
      case (k, _) => if(isGarbage(k)) {
        memory.store.remove(k)
      }
    }
  }

  def isGarbage(ref : JSReference) : Boolean = !markSet.contains(ref) && !ref.isBuiltIn
  def isGarbage(address : StackAddress) : Boolean = !markSetK.contains(address)

  def traceStack(stackAddress: StackAddress, memory: Memory) : Unit = {
    if(markSetK.contains(stackAddress)) return
    markSetK.add(stackAddress)
    for (GlobalFrame(_,_,_,next) <- memory.stack(stackAddress)) {
      traceStack(next, memory)
    }
  }

  def sweepStack(memory: Memory): Unit ={
    memory.stack.foreach {
      case (k, _) => if(isGarbage(k)) {
        //println("POP : " + k + "\n\n")
        memory.stack.remove(k)
      }
    }
  }

}
