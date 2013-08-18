package insynth.streams.ordered

import insynth.streams.{ Streamable, AddStreamable }

import insynth.util.logging.HasLogger

class LazyRoundRobbin[T](val initStreams: List[OrderedSizeStreamable[T]])
	extends OrderedSizeStreamable[T] with AddStreamable[T] with HasLogger {
  if (initStreams.isEmpty)
    println("creating initStreams is empty!!!")
  
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
  
  override def isInfinite = 
    if (initialized) initStreams.exists( _.isInfinite )//innerRoundRobbin.isInfinite
    else false
      
  private def getMinIndex = {
    
		val valueIterators = initStreams map { _.getValues.iterator.buffered }
    
    var min = Int.MaxValue
    var minInd = -1
    var ind = 0
    while (ind < valueIterators.size) {
      val indToCheck = ind % valueIterators.size
      
      if (valueIterators(indToCheck).hasNext && valueIterators(indToCheck).head < min) {
      	min = valueIterators(indToCheck).head
  			minInd = indToCheck
      }        
        
      ind += 1
    }
    
    //assert(minInd > -1, "minInd > -1")
    exiting("getMinIndex", (min, minInd).toString)
    (min, minInd)
  }
  
  lazy val (minValue, minInd) = getMinIndex
  
  lazy val mappedInitStreams = initStreams.zipWithIndex map {
    p =>
    	if (p._2 == minInd)
    	  SingleStream((p._1.getStream zip p._1.getValues).tail, p._1.isInfinite)
  	  else p._1
  }
    
  private def produceRoundRobbin = {
    if (innerRoundRobbin == null)
    	innerRoundRobbin = RoundRobbin[T]((mappedInitStreams ++ streams).toSeq)
  	innerRoundRobbin
  } 
  
  override def initialize = {    
    // important to first initialize
    // NOT??
    //produceRoundRobbin
    initialized = true
  }
    
  lazy val stream = 
    if (minInd > -1) initStreams(minInd).getStream.head #:: produceRoundRobbin.getStream
    else Stream.empty    
  
  override def getStream = {
    info("initialized " + initialized)
    
    if (initialized) stream
    else Stream.Empty
  }
  
  override def getValues = 
    if (initialized && minInd > -1) {
      assert(minInd > -1)
      minValue #:: produceRoundRobbin.getValues
    }
    else Stream.Empty
}

object LazyRoundRobbin {
	def apply[T](initStreams: List[OrderedSizeStreamable[T]]) = new LazyRoundRobbin(initStreams)
}