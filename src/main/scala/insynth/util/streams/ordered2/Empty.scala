package insynth.util.streams.ordered2

object Empty extends OrderedSizeStreamable[Nothing] {
    
  override def depleted: Boolean = true
  override def nextReady: Boolean = false
  
  override def getStream = Stream.empty  
  
  override def getValues = Stream.empty
  
}