import InfixOp._
import PrefixOp._
import UnaryAssignOp._

/**
  * Created by Fei Peng on 4/12/16.
  */
object JSSemantics {
  import AAM._
  import JSBuiltIn._
  def createFunctionObject(func : FunctionExpr, env : Environment, memory: Memory) : JSObject = {
    val res = memory.createEmptyObject(biFunctionProtoRef, biFunctionRef, func)
    val prototype = memory.createEmptyObject(biObjectProtoRef, alloc(func))
    if(func.prototypeID < 0) throw new RuntimeException("Invalid Prototype ID in: " + func)
    prototype.id = func.prototypeID
    val prototypeAddress = alloc(prototype.id)
    memory.putValue(prototypeAddress, prototype)
    res.content += (JSString(ConstantString("prototype")) -> prototypeAddress)
    res.code = JSClosure(func, env)
    res.generateFrom(func)
    res
  }

  def createRegExpObject(regexp : RegExp, memory: Memory) : JSObject = {
    val res = memory.createEmptyObject(biRegExpProtoRef, biRegExpRef)
    res.generateFrom(regexp)
    res
  }

  def createArrayObject(ka: KArrayComplete, elements : List[JSValue], memory: Memory) : JSObject = {
    val els = collection.mutable.Map.empty[JSString, JSReference]
    var i = 0
    elements.foreach {
      case ref : JSReference =>
        els += (JSString(ConstantString(i.toString)) -> ref)
        i += 1
      case wrong => throw new RuntimeException("Array Value :" + wrong + "is not Reference Based.")
    }
    els += JSString(ConstantString("length")) -> biArrayLengthRef
    val arrayObj = memory.createEmptyObject(biArrayProtoRef, biArrayRef, ka, els)
    arrayObj
  }

  def createStringObject(str: JSString) : JSObject = {
    val sObj =  JSObject(collection.mutable.Map(
      JSString(ConstantString("__proto__")) -> biStringProtoRef,
      JSString(ConstantString("constructor")) -> biStringRef,
      JSString(ConstantString("length")) -> biStringLength))
    sObj.primitiveValue = str
    sObj
  }
  def prefixFunc(op : PrefixOp, obj : JSValue) : JSValue = op match {
    case PrefixLNot => ToBoolean(obj) match {
      case JSBoolean(ConstantBoolean(false)) => JSBoolean(ConstantBoolean(true))
      case JSBoolean(ConstantBoolean(true)) => JSBoolean(ConstantBoolean(false))
      case JSBoolean(VariableBoolean) => JSBoolean(VariableBoolean)
    }
    case PrefixBNot => ToInt32(obj) match {
      case JSNumber(VariableNumber) => JSNumber(VariableNumber)
      case JSNumber(ConstantNumber(n)) => JSNumber(ConstantNumber(~n.toInt))
    }
    case PrefixMinus => ToNumber(obj) match {
      case JSNumber(ConstantNumber(n)) => JSNumber(ConstantNumber(n))
      case n => n
    }
    case PrefixPlus => ToNumber(obj)
    case PrefixVoid => JSUndefined
    case PrefixTypeof => obj match {
      case _ : JSString => JSString(ConstantString("string"))
      case _ : JSNumber => JSString(ConstantString("number"))
      case _ : JSBoolean => JSString(ConstantString("boolean"))
      case JSUndefined =>  JSString(ConstantString("undefined"))
      case JSNull =>  JSString(ConstantString("object"))
      case obj : JSObject =>
        if(isCallable(obj)) {
          JSString(ConstantString("function"))
        } else {
          JSString(ConstantString("object"))
        }
      case _ => throw new RuntimeException("Mismatch Type " + obj + "in PrefixTypeof.")
    }
    // TODO Delete
  }

  def infixFunc(op : InfixOp, e1 : JSValue, e2 : JSValue, memory: Memory) : JSValue = op match {
    case OpLT =>
      val p1 = ToPrimitive(e1)
      val p2 = ToPrimitive(e2)
      if(isString(p1) && isString(p2)) {
        p1 match {
          case JSString(ConstantString(s1)) => p2 match {
            case JSString(ConstantString(s2)) => JSBoolean(ConstantBoolean(s1 < s2))
            case JSString(VariableString) => JSBoolean(VariableBoolean)
            case _ => throw new RuntimeException("Mismatch " + p2 + "in OpLT.")
          }
          case JSString(VariableString) => JSBoolean(VariableBoolean)
          case _ => throw new RuntimeException("Cannot Reach this point.")
        }
      } else {
        val v1 = ToNumber(p1)
        val v2 = ToNumber(p2)
        if(v1.number == JSNaN || v2.number == JSNaN) {
          JSBoolean(ConstantBoolean(false))
        } else {
          v1 match {
            case JSNumber(ConstantNumber(n1)) => v2 match {
              case JSNumber(ConstantNumber(n2)) => JSBoolean(ConstantBoolean(n1 < n2))
              case JSNumber(VariableNumber) => JSBoolean(VariableBoolean)
            }
            case JSNumber(VariableNumber) => JSBoolean(VariableBoolean)
            case _ => throw new RuntimeException("Cannot Reach this point.")
          }
        }
      }

    case OpLEq =>
      val p1 = ToPrimitive(e1)
      val p2 = ToPrimitive(e2)
      if(isString(p1) && isString(p2)) {
        p1 match {
          case JSString(ConstantString(s1)) => p2 match {
            case JSString(ConstantString(s2)) => JSBoolean(ConstantBoolean(s1 <= s2))
            case JSString(VariableString) => JSBoolean(VariableBoolean)
            case _ => throw new RuntimeException("Cannot Reach this point.")
          }
          case JSString(VariableString) => JSBoolean(VariableBoolean)
          case _ => throw new RuntimeException("Cannot Reach this point.")
        }
      } else {
        val v1 = ToNumber(p1)
        val v2 = ToNumber(p2)
        if(v1.number == JSNaN || v2.number == JSNaN) {
          JSBoolean(ConstantBoolean(false))
        } else {
          v1 match {
            case JSNumber(ConstantNumber(n1)) => v2 match {
              case JSNumber(ConstantNumber(n2)) => JSBoolean(ConstantBoolean(n1 <= n2))
              case JSNumber(VariableNumber) => JSBoolean(VariableBoolean)
            }
            case JSNumber(VariableNumber) => JSBoolean(VariableBoolean)
            case _ => throw new RuntimeException("Cannot Reach this point.")
          }
        }
      }

    case OpGT =>
      val p1 = ToPrimitive(e1)
      val p2 = ToPrimitive(e2)
      if(isString(p1) && isString(p2)) {
        p1 match {
          case JSString(ConstantString(s1)) => p2 match {
            case JSString(ConstantString(s2)) => JSBoolean(ConstantBoolean(s1 > s2))
            case JSString(VariableString) => JSBoolean(VariableBoolean)
            case _ => throw new RuntimeException("Cannot Reach this point.")
          }
          case JSString(VariableString) => JSBoolean(VariableBoolean)
          case _ => throw new RuntimeException("Cannot Reach this point.")
        }
      } else {
        val v1 = ToNumber(p1)
        val v2 = ToNumber(p2)
        if(v1.number == JSNaN || v2.number == JSNaN) {
          JSBoolean(ConstantBoolean(false))
        } else {
          v1 match {
            case JSNumber(ConstantNumber(n1)) => v2 match {
              case JSNumber(ConstantNumber(n2)) => JSBoolean(ConstantBoolean(n1 > n2))
              case JSNumber(VariableNumber) => JSBoolean(VariableBoolean)
            }
            case JSNumber(VariableNumber) => JSBoolean(VariableBoolean)
            case _ => throw new RuntimeException("Cannot Reach this point.")
          }
        }
      }

    case OpGEq => //JSBoolean(VariableBoolean)/*
      val p1 = ToPrimitive(e1)
      val p2 = ToPrimitive(e2)
      if(isString(p1) && isString(p2)) {
        p1 match {
          case JSString(ConstantString(s1)) => p2 match {
            case JSString(ConstantString(s2)) => JSBoolean(ConstantBoolean(s1 >= s2))
            case JSString(VariableString) => JSBoolean(VariableBoolean)
          }
          case JSString(VariableString) => JSBoolean(VariableBoolean)
          case _ => throw new RuntimeException("Cannot Reach this point.")
        }
      } else {
        val v1 = ToNumber(p1)
        val v2 = ToNumber(p2)
        if(v1.number == JSNaN || v2.number == JSNaN) {
          JSBoolean(ConstantBoolean(false))
        } else {
          v1 match {
            case JSNumber(ConstantNumber(n1)) => v2 match {
              case JSNumber(ConstantNumber(n2)) => JSBoolean(ConstantBoolean(n1 >= n2))
              case JSNumber(VariableNumber) => JSBoolean(VariableBoolean)
            }
            case JSNumber(VariableNumber) => JSBoolean(VariableBoolean)
            case _ => throw new RuntimeException("Cannot Reach this point.")
          }
        }
      }

    case OpIn =>
      if(e2.isInstanceOf[JSObject]) {
         if(ToObject(e2).lookup(ToString(e1), memory).contains(cachedUndefined)) {
           JSBoolean(ConstantBoolean(false))
         } else {
           JSBoolean(ConstantBoolean(true))
         }
      } else {
         throw new RuntimeException(e2 + " is not Object.")
      }

    //TODO case OpInstanceof =>
    case OpEq => abstractEquality(e1, e2)

    case OpNEq => abstractEquality(e1, e2) match {
      case JSBoolean(ConstantBoolean(true)) => JSBoolean(ConstantBoolean(false))
      case JSBoolean(ConstantBoolean(false)) => JSBoolean(ConstantBoolean(true))
      case JSBoolean(VariableBoolean) => JSBoolean(VariableBoolean)
    }

    case OpStrictEq =>
      if(isVariableValue(e1) || isVariableValue(e2)) {
        JSBoolean(VariableBoolean)
      } else if(e1 == e2) {
        JSBoolean(ConstantBoolean(true))
      } else {
        JSBoolean(ConstantBoolean(false))
      }

    case OpStrictNEq =>
      if(isVariableValue(e1) || isVariableValue(e2)) {
        JSBoolean(VariableBoolean)
      } else if(e1 != e2) {
        JSBoolean(ConstantBoolean(true))
      } else {
        JSBoolean(ConstantBoolean(false))
      }

    case OpLAnd =>
      val b1 = ToBoolean(e1)
      b1 match {
        case JSBoolean(ConstantBoolean(false)) => e1
        case JSBoolean(ConstantBoolean(true)) => e2
        case JSBoolean(VariableBoolean) => JSBoolean(VariableBoolean)
          //TODO go to Top
      }

    case OpLOr =>
      val b1 = ToBoolean(e1)
      b1 match {
        case JSBoolean(ConstantBoolean(true)) => e1
        case JSBoolean(ConstantBoolean(false)) => e2
        case JSBoolean(VariableBoolean) => JSBoolean(VariableBoolean)
        //TODO go to Top
      }

    case OpMul =>
      val v1 = ToNumber(e1)
      val v2 = ToNumber(e2)
      if(v1.number == JSNaN || v2.number == JSNaN) {
        JSNumber(JSNaN)
      } else {
        v1 match {
          case JSNumber(ConstantNumber(n1)) => v2 match {
            case JSNumber(ConstantNumber(n2)) => JSNumber(ConstantNumber(n1 * n2))
            case JSNumber(VariableNumber) => JSNumber(VariableNumber)
          }
          case JSNumber(VariableNumber) => JSNumber(VariableNumber)
        }
      }

    case OpDiv =>
      val v1 = ToNumber(e1)
      val v2 = ToNumber(e2)
      if(v1.number == JSNaN || v2.number == JSNaN) {
        JSNumber(JSNaN)
      } else {
        v1 match {
          case JSNumber(ConstantNumber(n1)) => v2 match {
            case JSNumber(ConstantNumber(n2)) => JSNumber(ConstantNumber(n1 / n2))
            case JSNumber(VariableNumber) => JSNumber(VariableNumber)
          }
          case JSNumber(VariableNumber) => JSNumber(VariableNumber)
        }
      }

    case OpMod =>
      val v1 = ToNumber(e1)
      val v2 = ToNumber(e2)
      if(v1.number == JSNaN || v2.number == JSNaN) {
        JSNumber(JSNaN)
      } else {
        v1 match {
          case JSNumber(ConstantNumber(n1)) => v2 match {
            case JSNumber(ConstantNumber(n2)) => JSNumber(ConstantNumber(n1 % n2))
            case JSNumber(VariableNumber) => JSNumber(VariableNumber)
          }
          case JSNumber(VariableNumber) => JSNumber(VariableNumber)
        }
      }

    case OpLShift =>
      val v1 = ToInt32(e1)
      val v2 = ToInt32(e2)
      v1 match {
        case JSNumber(ConstantNumber(n1)) => v2 match {
          case JSNumber(ConstantNumber(n2)) => JSNumber(ConstantNumber(n1.toInt << n2.toInt))
          case _ => JSNumber(VariableNumber)
        }
        case _ => JSNumber(VariableNumber)
      }

    case OpSpRShift =>
      val v1 = ToInt32(e1)
      val v2 = ToInt32(e2)
      v1 match {
        case JSNumber(ConstantNumber(n1)) => v2 match {
          case JSNumber(ConstantNumber(n2)) => JSNumber(ConstantNumber(n1.toInt >> n2.toInt))
          case _ => JSNumber(VariableNumber)
        }
        case _ => JSNumber(VariableNumber)
      }
    case OpZfRShift =>
      val v1 = ToInt32(e1)
      val v2 = ToInt32(e2)
      v1 match {
        case JSNumber(ConstantNumber(n1)) => v2 match {
          case JSNumber(ConstantNumber(n2)) => JSNumber(ConstantNumber(n1.toInt >>> n2.toInt))
          case _ => JSNumber(VariableNumber)
        }
        case _ => JSNumber(VariableNumber)
      }

    case OpBAnd =>
      val v1 = ToInt32(e1)
      val v2 = ToInt32(e2)
      v1 match {
        case JSNumber(ConstantNumber(n1)) => v2 match {
          case JSNumber(ConstantNumber(n2)) => JSNumber(ConstantNumber(n1.toInt & n2.toInt))
          case _ => JSNumber(VariableNumber)
        }
        case _ => JSNumber(VariableNumber)
      }

    case OpBOr =>
      val v1 = ToInt32(e1)
      val v2 = ToInt32(e2)
      v1 match {
        case JSNumber(ConstantNumber(n1)) => v2 match {
          case JSNumber(ConstantNumber(n2)) => JSNumber(ConstantNumber(n1.toInt | n2.toInt))
          case _ => JSNumber(VariableNumber)
        }
        case _ => JSNumber(VariableNumber)
      }

    case OpBXor =>
      val v1 = ToInt32(e1)
      val v2 = ToInt32(e2)
      v1 match {
        case JSNumber(ConstantNumber(n1)) => v2 match {
          case JSNumber(ConstantNumber(n2)) => JSNumber(ConstantNumber(n1.toInt ^ n2.toInt))
          case _ => JSNumber(VariableNumber)
        }
        case _ => JSNumber(VariableNumber)
      }

    case OpAdd =>
      val p1 = ToPrimitive(e1)
      val p2 = ToPrimitive(e2)
      if (isString(p1) || isString(p2)) {
        val s1 = ToString(p1)
        val s2 = ToString(p2)
        s1 match {
          case JSString(ConstantString(cs1)) => s2 match {
            case JSString(ConstantString(cs2)) => JSString(ConstantString(cs1 + cs2))
            case JSString(VariableString) => JSString(VariableString)
          }
          case JSString(VariableString) => JSString(VariableString)
        }
      } else {
        val n1 = ToNumber(p1)
        val n2 = ToNumber(p2)
        n1 match {
          case JSNumber(ConstantNumber(cn1)) => n2 match {
            case JSNumber(ConstantNumber(cn2)) => JSNumber(ConstantNumber(cn1 + cn2))
            case JSNumber(JSNaN) => JSNumber(JSNaN)
            case _ => JSNumber(VariableNumber)
          }
          case JSNumber(JSNaN) => JSNumber(JSNaN)
          case JSNumber(VariableNumber) => n2 match {
            case JSNumber(JSNaN) => JSNumber(JSNaN)
            case _ => JSNumber(VariableNumber)
          }
        }
      }

    case OpSub =>
      //JSNumber(VariableNumber)
      val num1 = ToNumber(e1)
      val num2 = ToNumber(e2)
      if(num1.number == VariableNumber || num2.number == VariableNumber) {
        JSNumber(VariableNumber)
      } else if(num1.number == JSNaN || num2.number == JSNaN){
        JSNumber(JSNaN)
      } else {
        num1 match {
          case JSNumber(ConstantNumber(number1)) => num2 match {
            case JSNumber(ConstantNumber(number2)) => JSNumber(ConstantNumber(number1 - number2))
          }
        }
      }

    //TODO
  }

  def unaryAssignFunction(op : UnaryAssignOp, lv: JSValue, memory: Memory) : Set[JSNumber] = lv match {
      //TODO GetReferencedName(lhs) is either "eval" or "arguments"
    case ref : JSReference =>
      val oldValues = memory.getValue(ref).map(ToNumber(_))
      op match {
        case PostfixInc =>
          oldValues.foreach {
            case JSNumber(ConstantNumber(n)) => memory.putValue(ref, JSNumber(ConstantNumber(n + 1)))
            case _ =>
          }
          oldValues
        case PostfixDec =>
          oldValues.foreach {
            case JSNumber(ConstantNumber(n)) => memory.putValue(ref, JSNumber(ConstantNumber(n - 1)))
            case _ =>
          }
          oldValues
        case PrefixInc =>
          oldValues.map {
            case JSNumber(ConstantNumber(n)) =>
              val newVlaue = JSNumber(ConstantNumber(n + 1))
              memory.putValue(ref, newVlaue)
              newVlaue
            case num => num
          }
        case PrefixDec =>
          oldValues.map {
            case JSNumber(ConstantNumber(n)) =>
              val newValue = JSNumber(ConstantNumber(n + 1))
              memory.putValue(ref, newValue)
              newValue
            case num => num
          }
      }

    case _ => throw new RuntimeException("UnaryAssignFunction does not accept : " + lv)
  }

  def ToBoolean(obj: JSValue): JSBoolean = obj match {
    case b: JSBoolean => b
    case JSNumber(ConstantNumber(0)) => JSBoolean(ConstantBoolean(false))
    case JSNumber(ConstantNumber(_)) => JSBoolean(ConstantBoolean(true))
    case JSNumber(VariableNumber) => JSBoolean(VariableBoolean)
    case JSNumber(JSNaN) => JSBoolean(ConstantBoolean(false))
    case JSUndefined => JSBoolean(ConstantBoolean(false))
    case JSNull => JSBoolean(ConstantBoolean(false))
    case JSString(ConstantString("")) => JSBoolean(ConstantBoolean(false))
    case JSString(ConstantString(_)) | JSString(VariableString) => JSBoolean(ConstantBoolean(true))
    case JSObject(_) => JSBoolean(ConstantBoolean(true))
    case _ => throw new RuntimeException("Unknown value in ToBoolean.")
  }


  def ToNumber(obj: JSValue): JSNumber = obj match {
    case n: JSNumber => n
    case JSUndefined => JSNumber(JSNaN)
    case JSBoolean(ConstantBoolean(true)) => JSNumber(ConstantNumber(1))
    case JSBoolean(ConstantBoolean(false)) => JSNumber(ConstantNumber(0))
    case JSBoolean(VariableBoolean) => JSNumber(VariableNumber)
    case JSString(VariableString) => JSNumber(VariableNumber)
    case JSString(ConstantString(str)) =>
      try {
        val num = str.toDouble
        JSNumber(ConstantNumber(num))
      } catch {
        case _: Throwable => JSNumber(JSNaN)
      }
    case o: JSObject => JSNumber(JSNaN) //ToPrimitive
    case miss => throw new RuntimeException("Unknown value in ToNumber." + miss)
  }

  def ToObject(value : JSValue) : JSObject = value match {
    case _: JSReference => throw new RuntimeException("Reference :" + value +"cannot be converted to Object.")
    case obj : JSObject => obj
    case str : JSString => createStringObject(str)
    case _ => throw new RuntimeException("TODO ToObject with :" + value)
  }

  def canToObject(value : JSValue) : Boolean = value match {
    case JSNull => false
    case JSUndefined => false
    case _ => true
  }


  def ToString(obj: JSValue): JSString = obj match {
    case s: JSString => s
    case JSUndefined => JSString(ConstantString("undefined"))
    case JSNull => JSString(ConstantString("null"))
    case JSBoolean(ConstantBoolean(b)) => JSString(ConstantString(b.toString))
    case JSNumber(JSNaN) => JSString(ConstantString("NaN"))
    case JSNumber(ConstantNumber(n)) => numberToJSString(n)
    case JSNumber(VariableNumber) => JSString(VariableString)
    case JSObject(_) => JSString(VariableString)
    case _ => throw new RuntimeException("Unknown value in ToString." + obj)
  }


  def ToString(property: Property) : JSString = property match {
    case PropVar(name) => JSString(ConstantString(name))
    case PropString(name) => JSString(ConstantString(name))
    case PropNum(n) => numberToJSString(n)
  }

  def numberToJSString(n : Double) : JSString = {
    val iPart = n.toLong
    val mPart = n - iPart
    if(mPart == 0) {
      JSString(ConstantString(iPart.toString))
    } else {
      JSString(ConstantString(n.toString))
    }
  }

  def isString(obj : JSValue) : Boolean = obj.isInstanceOf[JSString]
  def isNumber(obj : JSValue) : Boolean = obj.isInstanceOf[JSNumber]
  def isBoolean(obj : JSValue) : Boolean = obj.isInstanceOf[JSBoolean]
  def isObject(obj : JSValue) : Boolean = obj.isInstanceOf[JSObject]

  def ToPrimitive(obj : JSValue) = obj match {
    case o : JSObject =>
      if(isCallable(o)) {
        JSString(ConstantString(o.code.function.sourceCode))
      } else if(o.primitiveValue != null) {
        ToString(o.primitiveValue)
      } else {
        JSString(ConstantString("[object Object]"))
      }

    case _ => obj
  }

  def isCallable(obj : JSValue) = obj match {
    case o : JSObject => o.code != null
    case _ => false
  }

  def isVariableValue(obj : JSValue) : Boolean = obj match {
    case JSNumber(VariableNumber) => true
    case JSBoolean(VariableBoolean) => true
    case JSString(VariableString) => true
    case _ => false
  }

  def ToInt32(obj : JSValue) : JSNumber = {
    ToNumber(obj) match {
      case JSNumber(JSNaN) => JSNumber(ConstantNumber(0))
      case JSNumber(VariableNumber) => JSNumber(VariableNumber)
      case JSNumber(ConstantNumber(num)) => JSNumber(ConstantNumber(num.toInt))
    }
  }

  def abstractEquality(e1 : JSValue, e2 : JSValue) : JSBoolean = {
    if(e1 == JSUndefined && e2 == JSUndefined) {
      JSBoolean(ConstantBoolean(true))
    } else if (e1 == JSNull && e2 == JSNull) {
      JSBoolean(ConstantBoolean(true))
    } else if(isNumber(e1) && isNumber(e2)) {
      e1 match {
        case JSNumber(JSNaN) => JSBoolean(ConstantBoolean(false))
        case JSNumber(ConstantNumber(n1)) => e2 match {
          case JSNumber(ConstantNumber(n2)) => JSBoolean(ConstantBoolean(n1 == n2))
          case JSNumber(VariableNumber) => JSBoolean(VariableBoolean)
          case JSNumber(JSNaN) => JSBoolean(ConstantBoolean(false))
          case _ => throw new RuntimeException("Cannot Reach this point.")
        }
        case JSNumber(VariableNumber) => JSBoolean(VariableBoolean)
        case _ => throw new RuntimeException("Cannot Reach this point.")
      }
    } else if(isString(e1) && isString(e2)) {
      e1 match {
        case JSString(ConstantString(s1)) => e2 match {
          case JSString(ConstantString(s2)) => JSBoolean(ConstantBoolean(s1 == s2))
          case JSString(VariableString) => JSBoolean(VariableBoolean)
          case _ => throw new RuntimeException("Cannot Reach this point.")
        }
        case JSString(VariableString) => JSBoolean(VariableBoolean)
        case _ => throw new RuntimeException("Cannot Reach this point.")
      }
    } else if(isBoolean(e1) && isBoolean(e2)) {
      e1 match {
        case JSBoolean(ConstantBoolean(b1)) => e2 match {
          case JSBoolean(ConstantBoolean(b2)) => JSBoolean(ConstantBoolean(b1 == b2))
          case JSBoolean(VariableBoolean) => JSBoolean(VariableBoolean)
          case _ => throw new RuntimeException("Cannot Reach this point.")
        }
        case JSBoolean(VariableBoolean) => JSBoolean(VariableBoolean)
        case _ => throw new RuntimeException("Cannot Reach this point.")
      }
    } else if(e1 == JSNull && e2 == JSUndefined) {
      JSBoolean(ConstantBoolean(true))
    } else if(e1 == JSUndefined && e2 == JSNull) {
      JSBoolean(ConstantBoolean(true))
    } else if(isNumber(e1) && isString(e2)) {
      abstractEquality(e1, ToNumber(e2))
    } else if(isNumber(e2) && isString(e1)) {
      abstractEquality(ToNumber(e1), e2)
    } else if(isBoolean(e1)) {
      abstractEquality(ToNumber(e1), e2)
    } else if(isBoolean(e2)) {
      abstractEquality(e1, ToNumber(e2))
    } else if((isString(e1) || isNumber(e1)) && isObject(e2)) {
      abstractEquality(e1, ToPrimitive(e2))
    } else if((isString(e2) || isNumber(e2)) && isObject(e1)) {
      abstractEquality(ToPrimitive(e1), e2)
    } else {
      JSBoolean(ConstantBoolean(false))
    }
  }
}
