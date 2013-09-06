package insynth.reconstruction.eager.combinator

import insynth.load.Declaration
import insynth.structures.{ SuccinctType => Type, _ }
import insynth.util.format._

/**
 * can return the type of the (sub)tree
 */
trait Typable {
  def getType: Type
}

/**
 * abstract tree node
 */
abstract class Node(tpe: Type) extends Typable {
  def getType: Type = tpe  
  
  def getDecls: Iterable[Declaration]
}

case class AbsNode(dec: Declaration) extends Node(dec.getType) {
	override def getDecls = Iterable(dec)
}

case class SimpleNode(decls:List[Declaration], tpe: Type, params:Map[Type, ContainerNode])
extends Node(tpe) {
  def getDecls = decls
  override def getParams = params
}

/**
 * container for tree nodes
 */
case class ContainerNode(var nodes:Set[Node]) {
  def addNode(node:Node) {
    nodes += node
  }  
  def getNodes = nodes
}