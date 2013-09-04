package ch.epfl.insynth.reconstruction.intermediate

import scala.text.Document
import ch.epfl.scala.trees.{ ScalaType => Type, _ }
import ch.epfl.insynth.print._
import ch.epfl.insynth.trees.FormatType
import ch.epfl.insynth.print.FormatHelpers.nestedBrackets
import ch.epfl.insynth.print.FormatHelpers.nestedParen
import ch.epfl.insynth.print.FormatHelpers.paren
import ch.epfl.insynth.print.FormatHelpers.seqToDoc
import ch.epfl.insynth.print.FormatHelpers.strToDoc
import ch.epfl.insynth.reconstruction.intermediate._
import ch.epfl.insynth.reconstruction.combinator.Declaration

/**
 * can return the type of the (sub)tree
 */
trait Typable {
  def getType: Type
}

/**
 * abstract tree node
 * is capable of returning its type and to format itself
 */
abstract class Node extends Typable

/**
 * a leaf node, descent down the tree finishes at a subclass of this node 
 */
abstract class Leaf(tpe: Type) extends Node {
  def getType = tpe
}

/**
 * variable node represents a variable which is introduced to the typing context
 * within the given expression tree (it an identifier in scope)
 */
case class Variable(tpe: Type, name: String) extends Leaf(tpe)

/**
 * identifier in scope
 * @param decl declaration with more information about the identifier 
 */
case class Identifier(tpe: Type, decl: Declaration) extends Leaf(tpe)

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
case class Application(tpe: Type, params: List[Set[Node]]) extends Node {
  def getType = tpe
  def getParams = params
}

/**
 * abstraction element introduces new variable into the typing context
 */
case class Abstraction(tpe: Type, vars: List[Variable], subTrees: Set[Node]) extends Node {
  def getType = tpe
}

/**
 * how are intermediate nodes formated into pretty print documents
 */
object FormatableIntermediate {
  def apply(node: Node) = new FormatableIntermediate(node)
}

class FormatableIntermediate(node: Node) extends Formatable {
  def toDocument = toDocument(node)
  
  def toDocument(node: Node): Document = {
    import FormatHelpers._

    node match {
      case Variable(tpe, name) => paren(name :: ": " :: tpe.toString) 
      case Identifier(tpe, dec) => dec.getSimpleName :: "(" :: FormatType(dec.getType).toDocument :: ")"
      case NullLeaf => "Null"
      case Application(tpe, params) => {
        val headDoc:Document = params.head.head match {
          case Variable(_, name) => name
          case n => toDocument(n)
        } 
        headDoc :/:
        paren(seqToDoc(params.tail, ",", 
		  {s: Set[Node] => 
          	s.toList match {
          	  case List(el) => (toDocument(el))
          	  case s:List[Node] => nestedBrackets(seqToDoc(s, "|", { f:Node => nestedParen(toDocument(f)) }))
          	}
		  }
        ))
      }
      case Abstraction(tpe, vars, subtrees) =>
        paren(
          paren(seqToDoc(vars, ",", {d: Variable => toDocument(d)})) :/: "=>" :/:
    	  nestedBrackets(toDocument(subtrees.head))
		)
    }
  }
}
