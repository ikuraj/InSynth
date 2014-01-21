package insynth.streams

import insynth.util.logging.HasLogger

trait Memoized[+T] extends Streamable[T] with HasLogger {
  
  // will memoize the stream (without forcing the computation)
  lazy val memoizedStream = super.getStream
  
  override abstract def getStream = memoizedStream
  
}