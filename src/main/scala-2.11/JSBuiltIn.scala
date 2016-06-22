import AAM.State

/**
  * Created by Fei Peng on 4/18/16.
  */

object JSBuiltIn {
  import scala.collection.mutable
  import AAM.{StackAddress, GlobalFrame}
  private val initMemory = new Memory(mutable.Map.empty[JSReference, Set[JSValue]], mutable.Map.empty[StackAddress, Set[GlobalFrame]])
  private var builtInID : Long = -1

  val startAddress = StackAddress(-1, -1)
  private val invalidAddress = StackAddress(-1, -1)
  private val invalidReference = JSReference(-1)
  private val emptyLocalStack = List.empty[Continuation]

  val cachedNullRef = JSReference(freshBuiltInID)
  val cachedUndefined = JSReference(freshBuiltInID)

  val biGlobalObjectRef = JSReference(freshBuiltInID)

  val biObjectProtoRef = JSReference(freshBuiltInID)
  val biObjectRef = JSReference(freshBuiltInID)
  val biFunctionProtoRef = JSReference(freshBuiltInID)
  val biFunctionRef = JSReference(freshBuiltInID)

  // Function Prototype
  val biApplyRef = JSReference(freshBuiltInID)
  val biArgumentsRef = JSReference(freshBuiltInID)
  val biCallRef = JSReference(freshBuiltInID)
  val biBindRef = JSReference(freshBuiltInID)
  val biNameRef = JSReference(freshBuiltInID)
  val biCallerRef = JSReference(freshBuiltInID)
  val biLengthRef = JSReference(freshBuiltInID)

  //Object Prototype
  val biHasOwnPropertyRef = JSReference(freshBuiltInID)
  val biIsPrototypeOfRef = JSReference(freshBuiltInID)
  val biPropertyIsEnumerableRef = JSReference(freshBuiltInID)
  val biToLocaleStringRef = JSReference(freshBuiltInID)
  val biValueOfRef = JSReference(freshBuiltInID)
  val biToStringRef = JSReference(freshBuiltInID)

  // RegExp
  val biRegExpRef = JSReference(freshBuiltInID)
  val biRegExpProtoRef = JSReference(freshBuiltInID)

  // Array
  val biArrayRef = JSReference(freshBuiltInID)
  val biArrayProtoRef = JSReference(freshBuiltInID)
  val biArrayLengthRef = JSReference(freshBuiltInID)

  //Math
  val biMathRef = JSReference(freshBuiltInID)
  val biPIRef = JSReference(freshBuiltInID)
  val bisinRef = JSReference(freshBuiltInID)
  val bisqrtRef = JSReference(freshBuiltInID)
  val biabsRef = JSReference(freshBuiltInID)


  val builtInEnv : AAM.Environment = Map(
    "Object" -> biObjectRef,
    "Function" -> biFunctionRef,
    "RegExp" -> biRegExpRef,
    "Array" -> biArrayRef,
    "global" -> biGlobalObjectRef,
    "Math" -> biMathRef
  )

  private def freshBuiltInID = {
    builtInID -= 1
    builtInID
  }

  val startFrame = GlobalFrame(invalidReference, emptyLocalStack, Map(), invalidAddress)

  def setBuiltIn(): Memory = {
    initMemory.store(cachedNullRef) = Set(JSNull)
    initMemory.store(cachedUndefined) = Set(JSUndefined)
    initMemory.store(biArrayLengthRef) = Set(JSNumber(ConstantNumber(0)))
    createObject
    createFunction
    createRegExp
    createGlobalObject
    createArray
    createMath
    initMemory.stack += startAddress -> Set(startFrame)
    initMemory
  }

  def createFunctionPrototype : JSObject = {
    val biFunctionPrototype = initMemory.createEmptyObject(biObjectProtoRef, biObjectRef)
    biFunctionPrototype.content += (JSString(ConstantString("apply")) -> biApplyRef)
    biFunctionPrototype.content += (JSString(ConstantString("arguments")) -> biArgumentsRef)
    biFunctionPrototype.content += (JSString(ConstantString("call")) -> biCallRef)
    biFunctionPrototype.content += (JSString(ConstantString("bind")) -> biBindRef)
    biFunctionPrototype.content += (JSString(ConstantString("name")) -> biNameRef)
    biFunctionPrototype.content += (JSString(ConstantString("caller")) -> biCallerRef)
    biFunctionPrototype.content += (JSString(ConstantString("length")) -> biLengthRef)
    biFunctionPrototype
  }

  def createObject : JSObject = {
    val biObjectPrototype = initMemory.createEmptyObject(cachedNullRef, biFunctionRef)
    initMemory.store(biObjectProtoRef) = Set(biObjectPrototype)
    val biObject = initMemory.createEmptyObject(biFunctionProtoRef, biFunctionRef)
    biObject.content += (JSString(ConstantString("prototype")) -> biObjectProtoRef)

    initMemory.store(biObjectRef) = Set(biObject)
    biObject
  }

  def createFunction : JSObject = {
    val biFunctionPrototype = createFunctionPrototype
    initMemory.store(biFunctionProtoRef) = Set(biFunctionPrototype)
    val biFunction = initMemory.createEmptyObject(biFunctionProtoRef, biFunctionRef)
    biFunction.content += (JSString(ConstantString("prototype")) -> biFunctionProtoRef)

    initMemory.store(biFunctionRef) = Set(biFunction)
    biFunction
  }

  def createGlobalObject : JSObject = {
    val globalObject = initMemory.createEmptyObject(biObjectProtoRef, biObjectRef)
    initMemory.store(biGlobalObjectRef) = Set(globalObject)
    globalObject
  }

  def createRegExpProto : JSObject = {
    val regExpProto = initMemory.createEmptyObject(biObjectProtoRef, biFunctionRef)
    //regExpProto.content += (JSString(ConstantString("compile")) -> ??)

    regExpProto
  }

  def createRegExp : JSObject = {
    val regExpProto = createRegExpProto
    initMemory.store(biRegExpProtoRef) = Set(regExpProto)
    val regExp = initMemory.createEmptyObject(biFunctionProtoRef, biFunctionRef)
    regExp.content += (JSString(ConstantString("prototype")) -> biRegExpProtoRef)
    /*
    { input: [Getter/Setter],
      multiline: [Getter/Setter],
      lastMatch: [Getter/Setter],
      lastParen: [Getter/Setter],
      leftContext: [Getter/Setter],
      rightContext: [Getter/Setter],
      '$1': [Getter/Setter],
      '$2': [Getter/Setter],
      '$3': [Getter/Setter],
      '$4': [Getter/Setter],
      '$5': [Getter/Setter],
      '$6': [Getter/Setter],
      '$7': [Getter/Setter],
      '$8': [Getter/Setter],
      '$9': [Getter/Setter] }
     */
    initMemory.store(biRegExpRef) = Set(regExp)
    regExp
  }

  def createArrayProto : JSObject = {
    val arrayProto = initMemory.createEmptyObject(biObjectProtoRef, biFunctionRef)
    arrayProto.content += (JSString(ConstantString("length")) -> biArrayLengthRef)
    arrayProto
  }

  def createArray : JSObject = {
    val arrayProto = createArrayProto
    initMemory.store(biArrayProtoRef) = Set(arrayProto)
    val array = initMemory.createEmptyObject(biFunctionProtoRef, biFunctionRef)
    array.content += (JSString(ConstantString("prototypr")) -> biArrayProtoRef)
    initMemory.store(biArrayRef) = Set(array)
    array
  }


  def createMath : JSObject = {
    val math = initMemory.createEmptyObject(biObjectProtoRef, biObjectRef)
    initMemory.store(biMathRef) =  Set(math)
    math.content += (JSString(ConstantString("PI")) -> biPIRef)
    initMemory.store(biPIRef) = Set(JSNumber(ConstantNumber(3.141592653589793)))
    math.content += (JSString(ConstantString("sin")) -> bisinRef)
    initMemory.store(bisinRef) = Set(builtInFuntion(bisinRef))
    math.content += (JSString(ConstantString("sqrt")) -> bisqrtRef)
    initMemory.store(bisqrtRef) = Set(builtInFuntion(bisqrtRef))
    math.content += (JSString(ConstantString("abs")) -> biabsRef)
    initMemory.store(biabsRef) = Set(builtInFuntion(biabsRef))
    math
  }

  private def builtInFuntion(ref : JSReference) : JSObject = {
    val obj = JSObject(collection.mutable.Map())
    obj.code = JSClosure(FunctionExpr(None, Nil, EmptyStmt()), Map())
    obj.builtIn = ref
    obj
  }

  def methodBuiltInCall(receiver : JSObject, method : JSObject, args : List[JSValue], state : State) : State = {
    val funcs = Set(bisqrtRef, bisinRef, biabsRef)
    method.builtIn match {
      case func if funcs.contains(func) => funcBuiltInCall(method, args, state)
    }
  }

  def funcBuiltInCall(func : JSObject, args : List[JSValue], state : State) : State = {
    func.builtIn match {
      case sin if sin == bisinRef =>
        val value = JSNumber(VariableNumber)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)

      case sqrt if sqrt == bisqrtRef =>
        val value = JSNumber(VariableNumber)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)

      case abs if abs == biabsRef =>
        val value = JSNumber(VariableNumber)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
    }
  }

}
