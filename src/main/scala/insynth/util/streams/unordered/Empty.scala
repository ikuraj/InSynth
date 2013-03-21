package insynth.util.streams.unordered

import insynth.util.streams.Streamable

object Empty extends Streamable[Nothing] {
  override def isInfinite = false
  override def getStream = Stream.empty  
}