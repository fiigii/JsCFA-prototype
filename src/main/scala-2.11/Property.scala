/**
  * Created by Fei Peng on 2/29/16.
  */
abstract class Property extends AbstractSyntaxTree

case class PropVar(name : String) extends Property
case class PropString(name : String) extends Property
case class PropNum(index : Double) extends Property

case class ObjectPair(property: Property, expr : Expression) extends AbstractSyntaxTree
