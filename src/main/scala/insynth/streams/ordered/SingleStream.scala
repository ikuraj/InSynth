package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ SingleStream => UnSingleStream }

// NOTE this would require ordered stream
protected class SingleStream[T](elements: => Stream[T], values: => Stream[Int], isInfiniteFlag: Boolean)
	extends OrderedStreamable[T] {
  
  def this(stream: => Stream[(T, Int)], isInfiniteFlag: Boolean) =
    this(stream map { _._1 }, stream map { _._2 }, isInfiniteFlag)
  
  override def isInfinite = isInfiniteFlag
  
  override def isDepleted: Boolean = throw new RuntimeException // wtv
  override def nextReady(ind: Int): Boolean = throw new RuntimeException
  
  override def getStream = elements
  
  override def getValues = values
    
}

protected object SingleStream {
  def apply[T](stream: => Stream[(T, Int)], isInfiniteFlag: Boolean = false) =
    new SingleStream(stream, isInfiniteFlag)
}