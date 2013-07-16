package insynth.util.streams.ordered2

import insynth.util.streams.unordered.{ UnaryStream => UnUnaryStream }

// give modifyVal only when a monotonic function, otherwise your computer will blow up!
class UnaryStream[T, U](val streamable: OrderedSizeStreamable[T], name: String, modify: T=>U, modifyVal: Option[Int => Int] = None/*= identity */)
	extends OrderedSizeStreamable[U] { 
  
  override def depleted: Boolean = streamable.depleted // wtv
  override def nextReady(ind: Int): Boolean = streamable.nextReady(ind)
    
  lazy val memoizedStream =
    streamable.getStream map { modify(_) }
  
  override def getStream = memoizedStream
  
  override def getValues = 
    modifyVal match {
	    case None => streamable.getValues
	    case Some(f) => streamable.getValues.map(f)
  	} 
  
  override def toString = name
}

object UnaryStream {
  def apply[T, U](streamable: OrderedSizeStreamable[T], name: String, modify: T=>U, modifyVal: Option[Int => Int] = None) =
    new UnaryStream(streamable, name: String, modify, modifyVal)
}