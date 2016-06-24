import AAM.State

/**
  * Created by Fei Peng on 4/18/16.
  */

object JSBuiltIn {
  import scala.collection.mutable
  import AAM.{StackAddress, GlobalFrame}
  import JSSemantics.ToString
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
  val bijoinRef = JSReference(freshBuiltInID)

  //Math
  val biMathRef = JSReference(freshBuiltInID)
  val biPIRef = JSReference(freshBuiltInID)
  val bisinRef = JSReference(freshBuiltInID)
  val bisqrtRef = JSReference(freshBuiltInID)
  val biabsRef = JSReference(freshBuiltInID)
  val bimaxRef = JSReference(freshBuiltInID)
  val bifloorRef = JSReference(freshBuiltInID)
  val biceilRef = JSReference(freshBuiltInID)
  val birandomRef = JSReference(freshBuiltInID)

  //String
  val biStringRef = JSReference(freshBuiltInID)
  val biStringProtoRef = JSReference(freshBuiltInID)
  val bicharAtRef = JSReference(freshBuiltInID)
  val biStringLength = JSReference(freshBuiltInID)
  val bifromCharCodeRef = JSReference(freshBuiltInID)
  val bicharCodeAtRef = JSReference(freshBuiltInID)
  val bisubstringRef = JSReference(freshBuiltInID)
  val bisplitRef = JSReference(freshBuiltInID)

  //Date
  val biDateRef = JSReference(freshBuiltInID)
  val biDateProtoRef = JSReference(freshBuiltInID)
  val bigetTimeRef = JSReference(freshBuiltInID)
  val bisetTimeRef = JSReference(freshBuiltInID)
  val bigetDateRef = JSReference(freshBuiltInID)
  val bigetDayRef = JSReference(freshBuiltInID)
  val bigetMonthRef = JSReference(freshBuiltInID)
  val bigetYearRef = JSReference(freshBuiltInID)
  val bigetHoursRef = JSReference(freshBuiltInID)
  val bigetMinutesRef = JSReference(freshBuiltInID)
  val bigetSecondsRef = JSReference(freshBuiltInID)
  val bigetTimezoneOffsetRef = JSReference(freshBuiltInID)



  val builtInEnv : AAM.Environment = Map(
    "undefined" -> cachedUndefined,
    "Object" -> biObjectRef,
    "Function" -> biFunctionRef,
    "RegExp" -> biRegExpRef,
    "Array" -> biArrayRef,
    "global" -> biGlobalObjectRef,
    "Math" -> biMathRef,
    "String" -> biStringRef,
    "Date" -> biDateRef
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
    createString
    createDate
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
    biObjectPrototype.id = biObjectProtoRef.label
    biObjectPrototype.content += (JSString(ConstantString("toString")) -> biToStringRef)
    initMemory.store(biToStringRef) = Set(builtInFuntion(biToStringRef))
    initMemory.store(biObjectProtoRef) = Set(biObjectPrototype)

    val biObject = initMemory.createEmptyObject(biFunctionProtoRef, biFunctionRef)
    biObject.content += (JSString(ConstantString("prototype")) -> biObjectProtoRef)
    biObject.builtIn = biObjectRef
    biObject.code = JSClosure(FunctionExpr(None, Nil, EmptyStmt()), Map())
    biObject.id = biObjectRef.label
    initMemory.store(biObjectRef) = Set(biObject)

    biObject
  }

  def createFunction : JSObject = {
    val biFunctionPrototype = createFunctionPrototype
    biFunctionPrototype.id = biFunctionProtoRef.label
    initMemory.store(biFunctionProtoRef) = Set(biFunctionPrototype)
    val biFunction = initMemory.createEmptyObject(biFunctionProtoRef, biFunctionRef)
    biFunction.content += (JSString(ConstantString("prototype")) -> biFunctionProtoRef)

    initMemory.store(biFunctionRef) = Set(biFunction)
    biFunction.builtIn = biFunctionRef
    biFunction.code = JSClosure(FunctionExpr(None, Nil, EmptyStmt()), Map())
    biFunction.id = biFunctionRef.label
    biFunction
  }

  def createGlobalObject : JSObject = {
    val globalObject = initMemory.createEmptyObject(biObjectProtoRef, biObjectRef)
    initMemory.store(biGlobalObjectRef) = Set(globalObject)
    globalObject.id = biGlobalObjectRef.label
    globalObject
  }

  def createRegExpProto : JSObject = {
    val regExpProto = initMemory.createEmptyObject(biObjectProtoRef, biFunctionRef)
    //regExpProto.content += (JSString(ConstantString("compile")) -> ??)

    regExpProto
  }

  def createRegExp : JSObject = {
    val regExpProto = createRegExpProto
    regExpProto.id = biRegExpProtoRef.label
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
    regExp.builtIn = biRegExpRef
    regExp.code = JSClosure(FunctionExpr(None, Nil, EmptyStmt()), Map())
    regExp.id = biRegExpRef.label
    regExp
  }

  def createArrayProto : JSObject = {
    val arrayProto = initMemory.createEmptyObject(biObjectProtoRef, biFunctionRef)
    arrayProto.content += (JSString(ConstantString("length")) -> biArrayLengthRef)
    initMemory.store(biArrayLengthRef) = Set(JSNumber(VariableNumber))
    arrayProto.content += (JSString(ConstantString("join")) -> bijoinRef)
    initMemory.store(bijoinRef) = Set(builtInFuntion(bijoinRef))
    arrayProto
  }

  def createArray : JSObject = {
    val arrayProto = createArrayProto
    arrayProto.id = biArrayProtoRef.label
    initMemory.store(biArrayProtoRef) = Set(arrayProto)
    val array = initMemory.createEmptyObject(biFunctionProtoRef, biFunctionRef)
    array.content += (JSString(ConstantString("prototype")) -> biArrayProtoRef)
    initMemory.store(biArrayRef) = Set(array)
    array.builtIn = biArrayRef
    array.code = JSClosure(FunctionExpr(None, Nil, EmptyStmt()), Map())
    array.id = biArrayRef.label
    array
  }


  def createMath : JSObject = {
    val math = initMemory.createEmptyObject(biObjectProtoRef, biObjectRef)
    math.id = biMathRef.label
    initMemory.store(biMathRef) =  Set(math)
    math.content += (JSString(ConstantString("PI")) -> biPIRef)
    initMemory.store(biPIRef) = Set(JSNumber(ConstantNumber(3.141592653589793)))
    math.content += (JSString(ConstantString("sin")) -> bisinRef)
    initMemory.store(bisinRef) = Set(builtInFuntion(bisinRef))
    math.content += (JSString(ConstantString("cos")) -> bisinRef)
    math.content += (JSString(ConstantString("pow")) -> bisinRef)

    math.content += (JSString(ConstantString("sqrt")) -> bisqrtRef)
    initMemory.store(bisqrtRef) = Set(builtInFuntion(bisqrtRef))
    math.content += (JSString(ConstantString("abs")) -> biabsRef)
    initMemory.store(biabsRef) = Set(builtInFuntion(biabsRef))
    math.content += (JSString(ConstantString("max")) -> bimaxRef)
    initMemory.store(bimaxRef) = Set(builtInFuntion(bimaxRef))
    math.content += (JSString(ConstantString("floor")) -> bifloorRef)
    initMemory.store(bifloorRef) = Set(builtInFuntion(bifloorRef))
    math.content += (JSString(ConstantString("ceil")) -> biceilRef)
    initMemory.store(biceilRef) = Set(builtInFuntion(biceilRef))
    math.content += (JSString(ConstantString("random")) -> birandomRef)
    initMemory.store(birandomRef) = Set(builtInFuntion(birandomRef))
    math
  }

  def createStringProto : JSObject = {
    val stringProto = initMemory.createEmptyObject(biObjectProtoRef, biFunctionRef)
    stringProto.content += (JSString(ConstantString("charAt")) -> bicharAtRef)
    initMemory.store(bicharAtRef) = Set(builtInFuntion(bicharAtRef))
    stringProto.content += (JSString(ConstantString("length")) -> biStringLength)
    initMemory.store(biStringLength) = Set(JSNumber(VariableNumber))
    stringProto.content += (JSString(ConstantString("fromCharCode")) -> bifromCharCodeRef)
    initMemory.store(bifromCharCodeRef) = Set(builtInFuntion(bifromCharCodeRef))
    stringProto.content += (JSString(ConstantString("charCodeAt")) -> bicharCodeAtRef)
    initMemory.store(bicharCodeAtRef) = Set(builtInFuntion(bicharCodeAtRef))
    stringProto.content += (JSString(ConstantString("substring")) -> bisubstringRef)
    initMemory.store(bisubstringRef) = Set(builtInFuntion(bisubstringRef))
    stringProto.content += (JSString(ConstantString("split")) -> bisplitRef)
    initMemory.store(bisplitRef) = Set(builtInFuntion(bisplitRef))
    stringProto
  }

  def createString : JSObject = {
    val stringProto = createStringProto
    stringProto.id = biStringProtoRef.label
    initMemory.store(biStringProtoRef) = Set(stringProto)
    val string = initMemory.createEmptyObject(biFunctionProtoRef, biFunctionRef)
    string.content += (JSString(ConstantString("prototype")) -> biStringProtoRef)
    initMemory.store(biStringRef) = Set(string)
    string.builtIn = biStringRef
    string.code = JSClosure(FunctionExpr(None, Nil, EmptyStmt()), Map())
    string.id = biStringRef.label
    string
  }

  def createDateProto : JSObject = {
    val dateProto = initMemory.createEmptyObject(biObjectProtoRef, biFunctionRef)
    dateProto.content += (JSString(ConstantString("getTime")) -> bigetTimeRef)
    initMemory.store(bigetTimeRef) = Set(builtInFuntion(bigetTimeRef))
    dateProto.content += (JSString(ConstantString("setTime")) -> bisetTimeRef)
    initMemory.store(bisetTimeRef) = Set(builtInFuntion(bisetTimeRef))
    dateProto.content += (JSString(ConstantString("getDate")) -> bigetDateRef)
    initMemory.store(bigetDateRef) = Set(builtInFuntion(bigetDateRef))
    dateProto.content += (JSString(ConstantString("getDay")) -> bigetDayRef)
    initMemory.store(bigetDayRef) = Set(builtInFuntion(bigetDayRef))
    dateProto.content += (JSString(ConstantString("getMonth")) -> bigetMonthRef)
    initMemory.store(bigetMonthRef) = Set(builtInFuntion(bigetMonthRef))
    dateProto.content += (JSString(ConstantString("getYear")) -> bigetYearRef)
    initMemory.store(bigetYearRef) = Set(builtInFuntion(bigetYearRef))
    dateProto.content += (JSString(ConstantString("getHours")) -> bigetHoursRef)
    initMemory.store(bigetHoursRef) = Set(builtInFuntion(bigetHoursRef))
    dateProto.content += (JSString(ConstantString("getMinutes")) -> bigetMinutesRef)
    initMemory.store(bigetMinutesRef) = Set(builtInFuntion(bigetMinutesRef))
    dateProto.content += (JSString(ConstantString("getSeconds")) -> bigetSecondsRef)
    initMemory.store(bigetSecondsRef) = Set(builtInFuntion(bigetSecondsRef))
    dateProto.content += (JSString(ConstantString("getTimezoneOffset")) -> bigetTimezoneOffsetRef)
    initMemory.store(bigetTimezoneOffsetRef) = Set(builtInFuntion(bigetTimezoneOffsetRef))
    dateProto
  }

  def createDate : JSObject = {
    val dateProto = createDateProto
    dateProto.id = biDateProtoRef.label
    initMemory.store(biDateProtoRef) = Set(dateProto)
    val date = initMemory.createEmptyObject(biFunctionProtoRef, biFunctionRef)
    date.content += (JSString(ConstantString("prototype")) -> biDateProtoRef)
    initMemory.store(biDateRef) = Set(date)
    date.builtIn = biDateRef
    date.code = JSClosure(FunctionExpr(None, Nil, EmptyStmt()), Map())
    date.id = biDateRef.label
    date
  }

  private def builtInFuntion(ref : JSReference) : JSObject = {
    val obj = JSObject(collection.mutable.Map())
    obj.code = JSClosure(FunctionExpr(None, Nil, EmptyStmt()), Map())
    obj.builtIn = ref
    obj
  }

  def newBuiltInCall(func : JSObject, args : List[JSValue], state : State) : State = func.builtIn match {
    case obj if obj == biObjectRef =>
      val value = JSObject(collection.mutable.Map(
        JSString(ConstantString("__proto__")) -> biObjectProtoRef,
        JSString(ConstantString("constructor")) -> biObjectRef
      ))
      value.generateFrom(state.e)
      val newMemory = state.memory.copy(state)
      val address = newMemory.save(value)
      State(address, state.env, state.localStack, state.a, newMemory)

    case array if array == biArrayRef =>
      val content = collection.mutable.Map.empty[JSString, JSReference]
      if(args.size > 1) {
        var i = 0
        args.foreach {
          case ref : JSReference =>
            content += (JSString(ConstantString(i.toString)) -> ref)
            i += 1
          case wrong => throw new RuntimeException("Array Value :" + wrong + "is not Reference Based.")
        }
        content += JSString(ConstantString("length")) -> biArrayLengthRef
      }
      content += (JSString(ConstantString("__proto__")) -> biArrayProtoRef)
      content += (JSString(ConstantString("constructor")) -> biArrayRef)
      val value = JSObject(content)
      value.generateFrom(state.e)
      val newMemory = state.memory.copy(state)
      val address = newMemory.save(value)
      State(address, state.env, state.localStack, state.a, newMemory)

    case date if date == biDateRef =>
      val date = JSObject(collection.mutable.Map.empty[JSString, JSReference])
      date.content += (JSString(ConstantString("__proto__")) -> biDateProtoRef)
      date.content += (JSString(ConstantString("constructor")) -> biDateRef)
      date.generateFrom(state.e)
      val newMemory = state.memory.copy(state)
      val address = newMemory.save(date)
      State(address, state.env, state.localStack, state.a, newMemory)
  }

  def methodBuiltInCall(receiver : JSObject, method : JSObject, args : List[JSValue], state : State) : State = {
    val funcs = Set(bisqrtRef, bisinRef, biabsRef, bimaxRef, bifloorRef, biceilRef, birandomRef)
    method.builtIn match {
      case func if funcs.contains(func) => funcBuiltInCall(method, args, state)
        //String
      case charAt if charAt == bicharAtRef  =>
        val value = JSString(VariableString)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
      case fromCharCode if fromCharCode == bifromCharCodeRef =>
        val value = JSString(VariableString)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
      case charCodeAt if charCodeAt == bicharCodeAtRef =>
        val value = JSNumber(VariableNumber)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
      case substring if substring == bisubstringRef =>
        val value = JSString(VariableString)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
      case split if split == bisplitRef =>
        val content = collection.mutable.Map(
          JSString(ConstantString("__proto__")) -> biArrayProtoRef,
          JSString(ConstantString("constructor")) -> biArrayRef
        )
        val value = JSObject(content)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
        //Object
      case toString if toString == biToStringRef =>
        val value = ToString(receiver)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
        //Array
      case join if join == bijoinRef =>
        val value = JSString(VariableString)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
        //Date
      case set if set == bisetTimeRef =>
        val value = JSNumber(VariableNumber)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
      case get if get == bigetTimeRef =>
        val value = JSNumber(VariableNumber)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
      case get if get == bigetDateRef || get == bigetMonthRef || get == bigetDayRef ||
        get == bigetYearRef || get == bigetHoursRef || get == bigetMinutesRef ||
        get == bigetSecondsRef || get == bigetTimezoneOffsetRef=>
        val value = JSNumber(VariableNumber)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
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

      case max if max == bimaxRef =>
        val value = JSNumber(VariableNumber)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
      case floor if floor == bifloorRef =>
        val value = JSNumber(VariableNumber)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
      case ceil if ceil == biceilRef =>
        val value = JSNumber(VariableNumber)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)
      case random if random == birandomRef =>
        val value = JSNumber(VariableNumber)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)

        //new
      case string if string == biStringRef =>
        val value = JSString(VariableString)
        value.generateFrom(state.e)
        val newMemory = state.memory.copy(state)
        val address = newMemory.save(value)
        State(address, state.env, state.localStack, state.a, newMemory)

      case array if array == biArrayRef =>
        newBuiltInCall(func, args, state)

      case obj if obj == biObjectRef =>
        newBuiltInCall(func, args, state)
    }
  }

}
