package insynth.util.format

import scala.text._
import insynth.util.streams.Streamable
import insynth.util.format._
import insynth.util.format.FormatHelpers.nestedBrackets
import insynth.util.format.FormatHelpers.paren
import insynth.util.format.FormatHelpers.sqBrackets
import insynth.util.format.FormatHelpers.strToDoc
import insynth.util.format.FormatHelpers.strToDoc
import insynth.util.streams.unordered.BinaryStream
import insynth.util.streams.unordered.Empty
import insynth.util.streams.unordered.RoundRobbin
import insynth.util.streams.unordered.SingleStream
import insynth.util.streams.unordered.Singleton
import insynth.util.streams.unordered.UnaryStream

/**
 * how are intermediate nodes formated into pretty print documents
 */
object FormatStreamUtils {
  def apply[_](node: Streamable[_]) = new FormatStreamUtils(node, -1)
  def apply[_](node: Streamable[_], level: Int) = new FormatStreamUtils(node, level: Int)
}

class FormatStreamUtils[_](node: Streamable[_], _level: Int) extends Formatable {
  
  import FormatHelpers._
  
  override def toDocument = toDocument(node, _level)
  
  def toDocument(node: Streamable[_], level: Int): Document = {
    trans(node, level, Set())
  }
  
  def printStreamsWithIndexForRoundRobbin(rr: RoundRobbin[_]) = nestedBrackets(
    ((DocNil: Document) /: (rr.streams map { s => strToDoc(rr.toString) } zip (0 to rr.streams.size))) {
    	(res, pair) => res :/: "," :/: paren(pair._1 :: "-" :: pair._2.toString)
    }
  )
  
  def trans(node: Streamable[_], level: Int, visited: Set[Streamable[_]]): Document = {
    
    if (level == 0)
      return DocNil
      
    if (visited contains node)
    	return sqBrackets(node.toString) 
    
    val resDocument: Document =
    node match {
      case _: Singleton[_] => "Singleton" :: sqBrackets(node.toString)
      case ss: SingleStream[_, _] => "SingleStream" :: sqBrackets(node.toString)// :: paren(trans(ss.stream, level - 1))
      case us: UnaryStream[_, _] => "UnaryStream" :: sqBrackets(node.toString) :: paren(trans(us.streamable, level - 1, visited + node))
      case rr: RoundRobbin[_] => "RoundRobin" :: sqBrackets(node.toString) :: nestedBrackets(
        ((DocNil: Document) /: rr.streams) { (res, doc) => res :/: trans(doc, level - 1, visited + node) }
      )
//      case irr: InitializingRoundRobin[_] => "InitializingRoundRobin" :: sqBrackets(node.toString) :: nestedBrackets(
//        ((DocNil: Document) /: irr.initStreams) { (res, doc) => res :/: trans(doc, level - 1, visited + node) }
//      ) ::
//      nestedBrackets(
//        ((DocNil: Document) /: (irr.streams)) {
//          (res, doc) => res :/: doc.asInstanceOf[Streamable[_]].getName
//        }
//      ) :: sqBrackets(irr.initialized.toString) :/:
//      	"innerRoundRobbin" :: 
//      	{	if (irr.innerRoundRobbin != null) 
//      	    irr.innerRoundRobbin.getName :: paren(printStreamsWithIndexForRoundRobbin(irr.innerRoundRobbin))
//    	    else
//    	      DocNil
//      	}
      case bs: BinaryStream[_, _, _] => "BinaryStream" :: sqBrackets(node.toString) :: nestedBrackets(
        paren(trans(bs.s1, level -1, visited + node)) :/: paren(trans(bs.s2, level -1, visited + node))
      )
      case Empty => "Empty"
      case _ => "ont know"
    }
    
    resDocument
    //:: brackets(node.getStream.isEmpty.toString)
  }
}
