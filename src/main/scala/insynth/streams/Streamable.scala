package insynth.streams

import org.sietf.logging.HasLogger

trait Streamable[+T] extends HasLogger {
  
  def isInfinite: Boolean
  def getStream: Stream[T]
  
}