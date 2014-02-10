package insynth.streams
package light

import scala.collection.mutable

import insynth.util.logging._

class RoundRobbinBuffer[T] protected[streams] (streams: Seq[Enumerable[T]])
	extends Enumerable[T] with HasLogger {
  
  private var streamsArray = mutable.ArrayBuffer(streams: _*)
  
  private def append(en: Enumerable[T]) {
    
  }
    
  override def apply(ind: Int) = {
    val arrInd = ind % streamsArray.size
    val elInd = ind / streamsArray.size
    streamsArray(arrInd)(elInd)
  }
    
  override def size =
    if (streams.exists(_.size == -1)) -1
    else streams.map(_.size).sum

}

object RoundRobbinBuffer {
  
  def apply[T](streams: Seq[Enumerable[T]]) =
    new RoundRobbinBuffer(streams)
  
}