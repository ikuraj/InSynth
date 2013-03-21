package insynth.reconstruction.intermediate

import insynth.leon.{ LeonDeclaration => Declaration }

import leon.purescala.TypeTrees.{ TypeTree => DomainType }

/**
 * can return the type of the (sub)tree
 */
trait Typable {
  def getType: DomainType
}

/**
 * abstract tree node
 * is capable of returning its type and to format itself
 */
abstract class Node extends Typable //with Nameable

/**
 * a leaf node, descent down the tree finishes at a subclass of this node 
 */
abstract class Leaf(tpe: DomainType) extends Node {
  def getType = tpe
}

/**
 * variable node represents a variable which is introduced to the typing context
 * within the given expression tree (it an identifier in scope)
 */
case class Variable(tpe: DomainType, name: String) extends Leaf(tpe)

/**
 * identifier in scope
 * @param decl declaration with more information about the identifier 
 */
case class Identifier(tpe: DomainType, decl: Declaration) extends Leaf(tpe)

/**
 * identifier in scope
 * @param decl declaration with more information about the identifier 
 */
case object NullLeaf extends Leaf(null)

/**
 * application term
 * first element in params is an expression (subtree) to which other parameters are
 * applied
 */
case class Application(tpe: DomainType, params: List[Set[Node]]) extends Node {
  var recursiveParams: List[Set[Node]] = Nil
  
  def getType = tpe
  def getParams = params
}

/**
 * abstraction element introduces new variable into the typing context
 */
case class Abstraction(tpe: DomainType, vars: List[Variable], subTrees: Set[Node]) extends Node {
  def getType = tpe
}

// for debugging

object Counter {
  private var counter = 0
  
  def getNewInt = { counter += 1; counter }
}

trait Nameable {  
  val myName = "interm_" + Counter.getNewInt
  
  //println("new Intermediate node created: " + myName)

  def getName = myName
  
  override def toString = getName  
}