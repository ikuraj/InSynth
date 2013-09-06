package insynth.reconstruction.eager.combinator

import insynth.load.Declaration
import insynth.structures.{ SuccinctType => Type, _ }
import insynth.util.format._

object FormatPrNode {
  def apply(node: Node) = new FormatPrNode(node: Node)
}

class FormatPrNode(node: Node) extends Formatable {
  import scala.text._

  def toDocument = toDocument(node)
  
  def toDocument(node: Any): scala.text.Document = {
    import FormatHelpers._

    node match {
      case AbsNode(tpe) => "Leaf" :: paren(tpe.toString)
      case SimpleNode(decls, tpe, map) =>
        "SimpleNode" :: paren(tpe.toString) :: nestedBrackets(
            seqToDoc(decls, ",", { d: Declaration => strToDoc(d.getSimpleName) })
            :/:
            seqToDoc(map.toList, ",", 
              { 
            	p: (Type, ContainerNode) => paren(p._1.toString) :: "->" ::
            	nestedBrackets(toDocument(p._2))
              }
            )
        )
      case ContainerNode(nodes) =>
        nestedBrackets(seqToDoc(nodes.toList, ",", toDocument(_:Node)))
        //"Container"
      // should not happen
      case _ => throw new RuntimeException
    }
  }
}

object FormatCombinations {
  
  def apply(comb: Combinations) = toDocument(comb)
  
  def toDocument(comb: Combinations): scala.text.Document = {
    import FormatHelpers._
    import scala.text.Document._

    comb match {
      case tree:Tree =>
        "Tree" :: paren(tree.tpe.toString) ::
        brackets( tree.getTraversalWeight.toString ) :/: 
        "[Done?" :: tree.isDone.toString :: "]" :: 
        nestedBrackets(
          seqToDoc(tree.decls.toList, ", ", { e:Expression => toDocument(e) })
        )
        //associatedTree: Tree, origDecl: Declaration, associatedNode: InSynth.SimpleNode
      case composite:Composite =>
        "Composite" :: paren(composite.origDecl.getSimpleName) ::
        brackets( composite.getTraversalWeight.toString ) :/:
        "[Done?" :: composite.isDone.toString :: "]" ::
        nestedBrackets(seqToDoc(composite.children.toList, ", ", { e:Tree => toDocument(e) }))
      case simple:Simple =>
        "Simple"  ::
        brackets( comb.getTraversalWeight.toString ) :: paren(simple.origDecl.getSimpleName)
      case leaf:LeafExpression =>
        "Leaf" :: brackets( comb.getTraversalWeight.toString )
    }
  }

}