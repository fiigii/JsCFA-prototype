/**
  * Created by Fei Peng on 5/21/16.
  */
import scala.collection.mutable
import AAM.{GlobalFrame, StackAddress, State, alloc, disk}
import JSBuiltIn.biObjectRef
import GarbageCollector.{markSet, markSetK, startGC}
import JSSemantics._

case class Memory(store : mutable.Map[JSReference, Set[JSValue]], stack : mutable.Map[StackAddress, Set[GlobalFrame]]) {

  def show : String = {
    val tmp = store.map {
      case (r, v) =>
        if (r.isBuiltIn) ""
        else r + " -> \n" + v + "\n"
    }
    tmp.mkString
  }

  def putValue(a : JSReference, v : JSValue) : Unit = {
    if (v.isInstanceOf[JSReference]) throw new RuntimeException("Cannot store a Reference.")
    if (store.contains(a)) {
      store(a) = mergeSet(store(a), v)
      mergeObject(a)
    } else {
      store += (a -> Set(v))
    }

    if (disk.contains(a)) {
      disk(a) = mergeSet(disk(a), v)
      mergeObjectInDisk(a)
    } else {
      disk += (a -> Set(v))
    }
  }

  def putSetValue(a: JSReference, set: Set[JSValue]): Unit = {
    if (store.contains(a)) {
      store(a) = set
    } else {
      store += (a -> set)
    }
  }


  def save(obj: JSValue): JSReference = {
    val add = alloc(obj)
    putValue(add, obj)
    add
  }


  def getValue(a: JSValue): Set[JSValue] = a match {
    case ref: JSReference => //store(ref)

    if(store.contains(ref)) {
      store(ref)
    } else {
      Set(JSUndefined)
    }


    case v => Set(v)
  }

  def globalFrames(a: StackAddress): Set[GlobalFrame] = stack(a)

  def pushGlobalStack(a: StackAddress, frame: GlobalFrame): Unit = {
    /*
    if(a == frame.a) {
      //println("loop")
      return
    } //detected loop
    */
    if (stack.contains(a)) {
      stack(a) += frame
    } else {
      stack(a) = Set(frame)
    }
  }


  def copy(state : State) : Memory ={
    startGC(state, this)
    val newStore = store.filter {
      case (ref@JSReference(r, _), _) => markSet.contains(ref) || r < 0
    }
    /*
    val copiedStore = newStore.map {
      case (ref, setValue) =>
        val newSet = setValue.map {
          case o : JSObject => o.copy
          case other => other
        }
        ref -> newSet
    } */

    //if(newStore.contains(JSReference(2022,2561))) println("JSReference(2022,2561) at \n" + state + "\n\n")

    val newStack = stack.filter {
      case (ref, _) => markSetK.contains(ref)
    }

    val newMemory = Memory(newStore, newStack)
    newMemory
  }

  def createEmptyObject(proto : JSReference, constr : JSReference, point : ObjectGeneratePoint = null, content : mutable.Map[JSString, JSReference] = mutable.Map.empty[JSString, JSReference]) : JSObject = {
    val biObjectPrototype = JSObject(content)
    if(point != null) {
      val protoAddr = alloc(point.protoID)
      for(v <- getValue(proto)) {
        putValue(protoAddr, v)
      }
      biObjectPrototype.content += (JSString(ConstantString("__proto__")) -> protoAddr)
      val consAddr = alloc(point.constructorID)
      for(v <- getValue(constr)) {
        putValue(consAddr, v)
      }
      biObjectPrototype.content += (JSString(ConstantString("constructor")) -> consAddr)
    } else {
      biObjectPrototype.content += (JSString(ConstantString("__proto__")) -> proto)
      biObjectPrototype.content += (JSString(ConstantString("constructor")) -> biObjectRef)
    }
    biObjectPrototype
  }

  private def mergeSet(oldSet: Set[JSValue], newValue: JSValue): Set[JSValue] = {
    if (oldSet.contains(newValue)) {
      oldSet
    } else {
      newValue match {
        case JSNumber(_) if oldSet.exists(_.isInstanceOf[JSNumber]) =>
          val noNumSet = oldSet.filterNot(_.isInstanceOf[JSNumber])
          noNumSet + JSNumber(VariableNumber)

        case JSBoolean(_) if oldSet.exists(_.isInstanceOf[JSBoolean]) =>
          val noBoolSet = oldSet.filterNot(_.isInstanceOf[JSBoolean])
          noBoolSet + JSBoolean(VariableBoolean)

        case JSString(_) if oldSet.exists(_.isInstanceOf[JSString]) =>
          val noStrSet = oldSet.filterNot(_.isInstanceOf[JSString])
          noStrSet + JSString(VariableString)

        case _ => oldSet + newValue
      }
    }
  }

  def mergeObject(ref: JSReference): Unit = {
    if (store.contains(ref) && store(ref).count(isObject(_)) > 1) {
      store(ref) = store(ref).toList.toSet
    }
  }

  def mergeObjectInDisk(ref: JSReference): Unit = {
    if (disk.contains(ref) && disk(ref).count(isObject(_)) > 1) {
      disk(ref) = disk(ref).toList.toSet
    }
  }

}
