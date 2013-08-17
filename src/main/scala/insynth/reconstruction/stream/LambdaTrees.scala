package insynth.reconstruction.stream

import insynth.leon.{ LeonDeclaration => Declaration }
import insynth.reconstruction.{ intermediate => int }

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

object Node {
  
  implicit def IntermediateToLambdaNode(inNode: int.Node): Node = 
	  inNode match {
    	case int.Identifier(tpe, decl) => Identifier(tpe, decl) 
    	case int.Variable(tpe, name) => Variable(tpe, name) 
    	case int.NullLeaf => NullLeaf
    	case _ => throw new RuntimeException
	  }  
  
  implicit def IntermediateIdentToLambdaNode(inNode: int.Identifier): Identifier = 
	  inNode match {
    	case int.Identifier(tpe, decl) => Identifier(tpe, decl) 
	  }
  
  implicit def IntermediateVarToLambdaNode(inNode: int.Variable): Variable = 
	  inNode match {
    	case int.Variable(tpe, name) => Variable(tpe, name) 
	  }
  
  def size(inNode: Node): Int = inNode match {
  	case _: Identifier | _: Variable | NullLeaf => 1
  	case Application(_, params) => 1 + params.tail.map(size).sum
  	case Abstraction(_, vars, body) => vars.size + size(body)
  	case _ => throw new RuntimeException    
  }
  
//  implicit def IntermediateToLambdaNode(inNode: int.NullLeaf): Node = 
//	  inNode match {
//    	case int.NullLeaf => NullLeaf 
//	  }
  
}

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
case class Application(tpe: DomainType, params: List[Node]) extends Node {  
  def getType = tpe
  def getParams = params
}

/**
 * abstraction element introduces new variable into the typing context
 */
case class Abstraction(tpe: DomainType, vars: List[Variable], subTrees: Node) extends Node {
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