package insynth.util.streams.ordered

import insynth.util.streams.unordered.{ SingleStream => UnSingleStream }

// NOTE this would require ordered stream
protected class SingleStream[T](stream: => Stream[(T, Int)], isInfiniteFlag: Boolean)
	extends UnSingleStream(stream map { _._1 }, isInfiniteFlag) with OrderedSizeStreamable[T] {
  
  override def getValues = stream map { _._2 }
    
}

protected object SingleStream {
  def apply[T](stream: => Stream[(T, Int)], isInfiniteFlag: Boolean = false) = new SingleStream(stream, isInfiniteFlag)
}