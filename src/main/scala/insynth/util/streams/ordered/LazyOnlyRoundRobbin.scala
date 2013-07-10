package insynth.util.streams.ordered

import insynth.util.streams.{ Streamable, AddStreamable }

import insynth.util.logging.HasLogger

class LazyOnlyRoundRobbin[T](name: String = "nebitnoLaz")
	extends OrderedSizeStreamable[T] with AddStreamable[T] with HasLogger {
  
  var initialized = false
      
  var streams: List[OrderedSizeStreamable[T]] = List.empty
  
  override def getStreams = streams
    
  var innerRoundRobbin: RoundRobbin[T] = _
  
  // XXX terrible hack, fix this
  override def addStreamable[U >: T](s: Streamable[U]) =
    streams :+= (s.asInstanceOf[OrderedSizeStreamable[T]])
  
  // XXX terrible hack, fix this
  override def addStreamable[U >: T](s: Iterable[Streamable[U]]) =
    streams ++= (s.asInstanceOf[Iterable[OrderedSizeStreamable[T]]])
  
  override def isInitialized = initialized
  
  override def isInfinite = true
          
  private def produceRoundRobbin = {
    if (innerRoundRobbin == null)
    	innerRoundRobbin = RoundRobbin[T](streams, name + " from LazyOnly")
  	innerRoundRobbin
  } 
  
  override def initialize = {    
    // important to first initialize
    // NOT??
    produceRoundRobbin
    initialized = true
  }   
  
  override def getStream = {
    fine("getStream LazyOnlyRoundRobbin")
    if (initialized) innerRoundRobbin.getStream
    else
      throw new RuntimeException
  }
  
  
  override def getValues =  {
    fine("getValues LazyRoundRobbin")
    if (initialized)
    	innerRoundRobbin.getValues
    else
      throw new RuntimeException
  }
    
  override def toString = name
}

object LazyOnlyRoundRobbin {
	def apply[T](name: String = "LazyDef") =
	  new LazyOnlyRoundRobbin(name)
}
