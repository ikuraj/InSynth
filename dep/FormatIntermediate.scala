package insynth.util.format

import scala.text._

import insynth.reconstruction.intermediate._

/**
 * how are intermediate nodes formated into pretty print documents
 */
object FormatIntermediate {
  def apply(node: Node) = new FormatIntermediate(node, -1)
  def apply(node: Node, level: Int) = new FormatIntermediate(node, level: Int)
  
  implicit def formatIndermediateToDocument(fi: FormatIntermediate) = fi.toDocument
  
  implicit def formatIndermediateToString(fi: FormatIntermediate) = fi.toDocument.toString
}

class FormatIntermediate(node: Node, _level: Int) extends Formatable {
  override def toDocument = toDocument(node, _level)
  
  def toDocument(node: Node, level: Int): Document = {
    import FormatHelpers._
    
    val resultDoc: Document =
    node match {
      case Variable(tpe, name) => "Var" :: paren(name/* :: ": " :: tpe.toString*/) 
      case Identifier(tpe, dec) => "Id" :: paren(dec.toString)
      case NullLeaf => "Null"
//      case app@Application(tpe, params) if level==0 => {
//        val headDoc: Document = params.head.head match {
//          case Variable(_, name) => name
//          case n => "..."
//        }
//        "App{[" :: (params map { _.size } mkString ",") :: "]" :: headDoc :/:
//          paren(seqToDoc(params.tail, ",",
//            { s: Set[Node] =>
//              s.toList match {
//                case List(v:Variable) => toDocument(v, 0)
//                case List(id:Identifier) => toDocument(id, 0)
//                case List(el) => strToDoc("...")
//                case s: List[_] => strToDoc("...")
//              }
//            })) :: sqBrackets(seqToDoc(app.recursiveParams, ",",
//            { s: Set[Node] =>
//              seqToDoc(s.toList, "|", { innerNode: Node => strToDoc(innerNode.getName) })
//            })) :: "}"
//      }
      case app@Application(tpe, params) => {
        val headDoc: Document = params.head.head match {
          case Variable(_, name) => name
          case n => toDocument(n, level-1)
        }
        "App{[" :: (params map { _.size } mkString ",") :: "]" :: headDoc :/:
          paren(seqToDoc(params.tail, ",",
            { s: Set[Node] =>
              s.toList match {
                case List(el) => (toDocument(el, level-1))
                case s: List[_] => nestedBrackets(seqToDoc(s, "|", { f: Node => nestedParen(toDocument(f, level-1)) }))
              }
            })) :: "recursive-" :: nestedBrackets(seqToDoc(app.recursiveParams, ",",
            { s: Set[Node] =>
              seqToDoc(s.toList, "|", { innerNode: Node => strToDoc(innerNode.toString) })
            })) :: "}"
      }
      case Abstraction(tpe, vars, subtrees) if level==0 =>
        "Abs" :: paren(
          paren(seqToDoc(vars, ",", { d: Variable => toDocument(d, 0) })) :/: "=> ..."
        )
      case Abstraction(tpe, vars, subtrees) =>
        "Abs" :: paren(
          paren(seqToDoc(vars, ",", { d: Variable => toDocument(d, level-1) })) :/: "=>" :/:
            nestedBrackets(toDocument(subtrees.head, level-1)))
    }
        
    sqBrackets(node.toString) :: resultDoc
  }
}
