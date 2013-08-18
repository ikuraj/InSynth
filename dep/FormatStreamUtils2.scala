package insynth.util.format

import scala.text._
import insynth.util.format._
import insynth.util.format.FormatHelpers.nestedBrackets
import insynth.util.format.FormatHelpers.paren
import insynth.util.format.FormatHelpers.sqBrackets
import insynth.util.format.FormatHelpers.strToDoc
import insynth.util.format.FormatHelpers.strToDoc
import insynth.util.streams.ordered2._
import insynth.util.streams.ordered2.{ OrderedSizeStreamable => Streamable }

/**
 * how are intermediate nodes formated into pretty print documents
 */
object FormatStreamUtils2 {
  def apply[_](node: Streamable[_]) = new FormatStreamUtils2(node, -1)
  def apply[_](node: Streamable[_], level: Int) = new FormatStreamUtils2(node, level: Int)
}

class FormatStreamUtils2[_](node: Streamable[_], _level: Int) extends Formatable {
  
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
    
    def header(node: Streamable[_]) = {
      sqBrackets(node.toString + "[" + /*"Red?" + node.nextReady + */"Dep?"/* + node.depleted */+ "]")
    }
    
    if (level == 0)
      return DocNil
      
    if (visited contains node)
    	return "Recursion: " :: header(node)
    
    val resDocument: Document =
    node match {
      case _: Singleton[_] => "Singleton" :: header(node)
      case ss: SingleStream[_] => "SingleStream" :: header(node)// :: paren(trans(ss.stream, level - 1))
      case us: UnaryStream[_, _] => "UnaryStream" :: header(node) ::
      	paren(trans(us.streamable, level - 1, visited + node))
//      case irr: InitializingRoundRobin[_] => "InitializingRoundRobin" :: header(node) :: nestedBrackets(
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
      case bs: BinaryStream[_, _, _] => "BinaryStream" :: header(node) :: nestedBrackets(
        paren(trans(bs.s1, level -1, visited + node)) :/: paren(trans(bs.s2, level -1, visited + node))
      )
      case Empty => "Empty"
      case lrr: LazyRoundRobbin[_] => "LazyRoundRobbin" :: header(node) :: nestedBrackets(
        ((DocNil: Document) /: lrr.getStreams) { (res, doc) => res :/: trans(doc, level - 1, visited + node) }
//        trans(lrr.innerRoundRobbin, level -1, visited + node)
      )      case rr: RoundRobbin[_] => "RoundRobin" :: header(node) :: nestedBrackets(
        ((DocNil: Document) /: rr.streams) { (res, doc) => res :/: trans(doc, level - 1, visited + node) }
      )
//      case rr: insynth.util.streams.ordered.RoundRobbin[_] => "ord.RoundRobin" :: header(node) :: nestedBrackets(
//        ((DocNil: Document) /: rr.streams) { (res, doc) => res :/: trans(doc, level - 1, visited + node) }
//      )
        
      case _ => throw new RuntimeException//"Dont know: " :: header(node)
    }
    
    resDocument// :: brackets("Inf?" + node.isInfinite)
  }
}
