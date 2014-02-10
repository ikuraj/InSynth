package insynth.streams
package light

import scala.collection.mutable

import insynth.util.logging._

class RoundRobbin[T] protected[streams] (streams: Seq[Enumerable[T]])
	extends Enumerable[T] with HasLogger {
  
  val streamsArray = streams.toArray
  
  override def apply(ind: Int) = {
    val arrInd = ind % streamsArray.size
    val elInd = ind / streamsArray.size
    streamsArray(arrInd)(elInd)
  }
    
  override def size =
    if (streams.exists(_.size == -1)) -1
    else streams.map(_.size).sum

}

object RoundRobbin {
  
  def apply[T](streams: Seq[Enumerable[T]]) =
    new RoundRobbin(streams)
  
}