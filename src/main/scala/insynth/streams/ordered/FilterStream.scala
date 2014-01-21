package insynth.streams.ordered

import insynth.streams._
import insynth.streams.unordered.{ UnaryStream => UnUnaryStream }

import scala.annotation.tailrec

class FilterStream[T](val streamable: IntegerWeightStreamable[T], filterFun: T => Boolean)
	extends IntegerWeightStreamable[T] {
  
  override def getValuedStream = streamable.getValuedStream filter { p => filterFun(p._1) }
  
  override def size = -1
  
}

object FilterStream {
  def apply[T, U](streamable: IntegerWeightStreamable[T], filterFun: T => Boolean) =
    new FilterStream(streamable, filterFun)
}