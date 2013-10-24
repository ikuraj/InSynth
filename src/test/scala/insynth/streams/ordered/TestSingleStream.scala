package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ SingleStream => UnSingleStream }

class TestSingleStream[T](stream: => Stream[(T, Int)], isInfiniteFlag: Boolean)
	extends SingleStream[T](stream, isInfiniteFlag) {
  
  val itVal = (stream map { _._2 }).iterator.buffered
  val itStr = (stream map { _._1 }).iterator.buffered
  
  override def isInfinite = isInfiniteFlag
  
  override def isDepleted: Boolean =
    false
      
  override def nextReady(ind: Int): Boolean =
    if (isInfiniteFlag) true
    else ind < stream.size
  
  override def getStream = stream map { _._1 }
  
  override def getValues = stream map { _._2 }
    
}

protected object TestSingleStream {
  def apply[T](stream: => Stream[(T, Int)], isInfiniteFlag: Boolean = false) =
    new TestSingleStream(stream, isInfiniteFlag)
}

class TestSingleStreamFixedSize[T](stream: => Stream[T], values: => Stream[Int], size: Int)
	extends SingleStream[T](stream, values, false) {
        
  override def isInfinite = false
  
  override def isDepleted: Boolean =
    false
      
  override def nextReady(ind: Int): Boolean =
    ind < size
  
  override def getStream = stream
  
  override def getValues = values
    
}

protected object TestSingleStreamFixedSize {
  def apply[T](stream: => Stream[T], values: => Stream[Int], size: Int) =
    new TestSingleStreamFixedSize(stream, values, size)
}