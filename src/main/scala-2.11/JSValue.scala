import AAM._
import JSSemantics._

import scala.collection.mutable

/**
  * Created by Fei Peng on 3/30/16.
  */

sealed abstract class JSValue extends AbstractSyntaxTree

case class JSReference(label: AbstractSyntaxTree.Label, var atObject : AbstractSyntaxTree.Label = -1) extends JSValue {
  def isBuiltIn = label < 0
}

case object JSNull extends JSValue
case object JSUndefined extends JSValue
case class JSNumber(number : AbstractNumber) extends JSValue
case class JSBoolean(bool : AbstractBoolean) extends JSValue
case class JSString(str : AbstractString) extends JSValue

case class KObjectPairPack(name : JSString, v : JSValue) extends JSValue {}

case class JSClosure(function: FunctionExpr, env : AAM.Environment)

case class JSObject(content : scala.collection.mutable.Map[JSString, JSReference]) extends JSValue {
  var code : JSClosure = null
  def lookup(field : JSString, memory: Memory) : Set[JSReference] = {
    val proto = JSString(ConstantString("__proto__"))
    if(content.contains(field)) {
      Set(content(field))
    } else if(content.contains(proto)) {
      val protoRef = content(proto)
      memory.getValue(protoRef).flatMap({
        case p : JSObject => p.lookup(field, memory)
        case JSNull => Set(JSBuiltIn.cachedUndefined)
        case error => throw new RuntimeException("Type Error : " + error)
      })
    } else {
      Set(JSBuiltIn.cachedUndefined)
    }
  }

  def lookupOwn(field : JSString) : JSReference = {
    content.get(field) match {
      case None => JSBuiltIn.cachedUndefined
      case Some(ref) => ref
    }
  }

  def hasOwnProperty(prop : JSString) : Boolean = content.contains(prop)

  def addField(prop : JSString, value : JSValue, obj : JSValue, memory: Memory): Unit = value match {
    case ref : JSReference =>
      ref.atObject = this.id
      this.content += (prop -> ref)
      mergeObject(obj, memory)
    case _ => throw new RuntimeException("Cannot add non-reference value to object : " + value)
  }

  def deleteField(prop: JSString, obj: JSValue, memory: Memory): Unit = prop match {
    case JSString(ConstantString(_)) =>
      this.content -= prop
      mergeObject(obj, memory)
    case _ =>
  }

  def keys(memory: Memory) : List[JSString] = {
    val ownKeys = content.keys.filterNot {
      case JSString(ConstantString(s)) =>
        List("__proto__", "prototype", "constructor").contains(s)
      case _ => false
    }
    val proto = lookupOwn(JSString(ConstantString("__proto__")))
    if(proto == JSBuiltIn.cachedUndefined || memory.getValue(proto).contains(JSNull)) {
      ownKeys.toList
    } else {
      //println("\n\nproto : " + proto + "\n\n")
      val tmp = for {
        theProto <- memory.getValue(proto)
        if theProto != JSNull
        uplevelKey <- ToObject(theProto).keys(memory)
      } yield uplevelKey
      ownKeys.toList ++ tmp
    }
  }

  def copy : JSObject = {
    val obj = JSObject(mutable.Map(this.content.toList:_*))
    obj.code = this.code
    if(this.hasID)obj.generateFrom(this)
    obj
  }

  private def mergeObject(v: JSValue, memory: Memory): Unit = v match {
    case ref: JSReference if memory.store.contains(ref) && memory.store(ref).count(isObject(_)) > 1 =>
      memory.store(ref) = memory.store(ref).toList.toSet
    case _ =>
  }

  override def toString = {
    if(code != null) {
      "FunctionObject : \n" + code.function.sourceCode
    } else {
      val strArr = content.map {
        case (k, v) => "        " + k + " -> " + v + "\n"
      }

      "Object : \n" + strArr.mkString
    }
  }
}


sealed abstract class AbstractNumber
case class ConstantNumber(value : Double) extends AbstractNumber
case object VariableNumber extends AbstractNumber
case object JSNaN extends AbstractNumber

sealed abstract class AbstractBoolean
case class ConstantBoolean(value : Boolean) extends AbstractBoolean
case object VariableBoolean extends AbstractBoolean

sealed abstract class AbstractString
case class ConstantString(value : String) extends AbstractString
case object VariableString extends AbstractString
