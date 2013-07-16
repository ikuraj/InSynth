package insynth.util.streams.ordered2

import insynth.util.streams.unordered.{ SingleStream => UnSingleStream }

// NOTE this would require ordered stream
protected class SingleStream[T](stream: => Stream[(T, Int)])
	extends OrderedSizeStreamable[T] {
  
  override def depleted: Boolean = throw new RuntimeException // wtv
  override def nextReady(ind: Int): Boolean = throw new RuntimeException
  
  override def getStream = stream map { _._1 }
  
  override def getValues = stream map { _._2 }
    
}

protected object SingleStream {
  def apply[T](stream: => Stream[(T, Int)], isInfiniteFlag: Boolean = false) =
    new SingleStream(stream)
}