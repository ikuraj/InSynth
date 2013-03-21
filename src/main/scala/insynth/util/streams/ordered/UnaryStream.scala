package insynth.util.streams.ordered

import insynth.util.streams.unordered.{ UnaryStream => UnUnaryStream }

// give modifyVal only when a monotonic function, otherwise your computer will blow up!
class UnaryStream[T, U](override val streamable: OrderedSizeStreamable[T], modify: T=>U, modifyVal: Option[Int => Int] = None/*= identity */)
	extends UnUnaryStream(streamable, modify) with OrderedSizeStreamable[U] {  
  
  override def getValues = 
    modifyVal match {
	    case None => streamable.getValues
	    case Some(f) => streamable.getValues.map(f)
  	} 
  
}

object UnaryStream {
  def apply[T, U](streamable: OrderedSizeStreamable[T], modify: T=>U, modifyVal: Option[Int => Int] = None) =
    new UnaryStream(streamable, modify, modifyVal)
}