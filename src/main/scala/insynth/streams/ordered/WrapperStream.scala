package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ SingleStream => UnSingleStream }

/**
 * Wrapper around the Scala stream
 * NOTE: parameter stream needs to be ordered itself
 */
class WrapperStream[T](stream: => Stream[(T, Int)])
	extends IntegerWeightStreamable[T] {
  
  override def isInfinite = true
  
  override def getValuedStream = stream
    
  override def size = -1
}

object WrapperStream {
  def apply[T](stream: => Seq[(T, Int)]) =
    if (stream.hasDefiniteSize)
      new FiniteStream(stream)
    else
      new WrapperStream(stream.toStream)

  def apply[T](stream: => Stream[(T, Int)], isInfinite: Boolean) =
    if (isInfinite)
      new WrapperStream(stream.toStream)
    else
      new FiniteStream(stream.toList)

  def apply(el: => Int) =
    new Singleton(el, el)

  def counted[T](stream: => Seq[(T, Int)]) =
    if (stream.hasDefiniteSize)
      new FiniteStream(stream) with Counted[T]
    else
      new WrapperStream(stream.toStream) with Counted[T]

  def counted[T](stream: => Stream[(T, Int)], isInfinite: Boolean) =
    if (isInfinite)
      new WrapperStream(stream.toStream) with Counted[T]
    else
      new FiniteStream(stream.toList) with Counted[T]

  def counted(el: => Int) =
    new Singleton(el, el) with Counted[Int]
}