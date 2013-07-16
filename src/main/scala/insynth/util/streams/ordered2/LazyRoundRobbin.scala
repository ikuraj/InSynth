package insynth.util.streams.ordered2

import insynth.util.logging.HasLogger

class LazyRoundRobbin[T](val initStreamsIn: List[OrderedSizeStreamable[T]], name: String = "nebitnoLaz")
	extends OrderedSizeStreamable[T] with AddStreamable[T] with HasLogger {
    
  override def depleted: Boolean = 
    if (initialized) innerRoundRobbin.depleted
    else false
    
  override def nextReady(ind: Int): Boolean = 
    if (initialized) innerRoundRobbin.nextReady(ind)
    else false
  
  var initialized = false
      
  var streams: List[OrderedSizeStreamable[T]] = initStreamsIn
  
  override def getStreams = streams
    
  var innerRoundRobbin: RoundRobbin[T] = _
  
  // XXX terrible hack, fix this
  override def addStreamable[U >: T](s: OrderedSizeStreamable[U]) =
    streams :+= (s.asInstanceOf[OrderedSizeStreamable[T]])
  
  // XXX terrible hack, fix this
  override def addStreamable[U >: T](s: Iterable[OrderedSizeStreamable[U]]) =
    streams ++= (s.asInstanceOf[Iterable[OrderedSizeStreamable[T]]])
  
  override def isInitialized = initialized
    
  private def produceRoundRobbin = {
    if (innerRoundRobbin == null)
    	innerRoundRobbin = RoundRobbin[T](getStreams, name + " from LazyRound")
  	innerRoundRobbin
  } 
  
  override def initialize = {    
    produceRoundRobbin
    initialized = true
  }
    
  lazy val stream = produceRoundRobbin.getStream
  
  override def getStream = stream
  
  override def getValues = produceRoundRobbin.getValues
    
  override def toString = name
}

object LazyRoundRobbin {
	def apply[T](initStreams: List[OrderedSizeStreamable[T]], name: String = "LazyDef") =
	  new LazyRoundRobbin(initStreams, name)
}
