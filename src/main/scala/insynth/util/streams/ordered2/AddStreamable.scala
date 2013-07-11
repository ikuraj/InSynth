package insynth.util.streams.ordered2

import insynth.util.logging.HasLogger

trait AddStreamable[+T] {
  def addStreamable[U >: T](s: OrderedSizeStreamable[U])
  
  def addStreamable[U >: T](s: Iterable[OrderedSizeStreamable[U]])
  
  def isInitialized: Boolean
  
  def initialize: Unit
  
  def getStreams: List[OrderedSizeStreamable[T]]
}