package insynth.structures

import insynth.structures.{ SuccinctType => Type }
import insynth.load.Declaration

import scala.collection.mutable.{ Set, Map }

trait Node

class SimpleNode(decls:List[Declaration], params:Map[Type, ContainerNode]) extends Node {
  def getDecls = decls
  def getParams = params
}

object SimpleNode {
  
  def apply(decls:List[Declaration], params:Map[Type, ContainerNode]) =
    new SimpleNode(decls, params)  
}

/**
 * container for tree nodes
 */
class ContainerNode(var nodes: Set[SimpleNode]) extends Node {
  
  def this() = this(Set.empty)
  
  def addNode(node:SimpleNode){
    nodes += node
  }
  
  def getNodes = nodes
}

object ContainerNode {
  
  def apply(nodes: Set[SimpleNode]) = new ContainerNode(nodes)
  
}