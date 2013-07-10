package insynth.util.streams.ordered

import scala.collection.mutable.{ Seq => MutableSeq }

import insynth.util.logging.HasLogger
import insynth.util.streams.ordered.{ OrderedSizeStreamable => Streamable }
import insynth.util.streams.unordered.{ RoundRobbin => UnRoundRobbin }

class CareRoundRobbin[T] protected[streams] (override val streams: Seq[Streamable[T]], name: String = "nebitno")
	extends UnRoundRobbin(streams) with Streamable[T] with HasLogger {
  
  assert(streams.exists(! _.isInfinite), "streams.exists(! _.isInfinite)")
  val (infStreams, finStreams) = streams.partition(_.isInfinite)
  
  override def isInfinite = false
  
//  var innerRoundRobbin: RoundRobbin[T] = _
  
  private def getMinIndex = {
		val valueIterators = finStreams map { _.getValues.iterator.buffered }
    
    var min = Int.MaxValue
    var minInd = -1
    var ind = 0
    while (ind < valueIterators.size) {
      val indToCheck = ind % valueIterators.size
  		fine("getMinIndex in CaseRoundRobbin, indToCheck=" + indToCheck)
      
      if (valueIterators(indToCheck).hasNext && valueIterators(indToCheck).head < min) {
      	min = valueIterators(indToCheck).head
  			minInd = indToCheck
      }        
        
      ind += 1
    }
    
    assert(minInd > -1, "minInd > -1")
    exiting("getMinIndex", (min, minInd).toString)
    (min, minInd)
  }
  
  lazy val (minValue, minInd) = getMinIndex
  
  lazy val mappedFinStreams = finStreams.zipWithIndex map {
    p =>
    	if (p._2 == minInd)
    	  SingleStream((p._1.getStream zip p._1.getValues).tail, p._1.isInfinite)
  	  else p._1
  }
    
  lazy val innerRoundRobbin = {
//    throw new RuntimeException
    RoundRobbin[T](mappedFinStreams ++ infStreams, name + " fromCare")
  }
//  private def produceRoundRobbin = {
//    if (innerRoundRobbin == null)
//    	innerRoundRobbin = RoundRobbin[T](mappedFinStreams ++ infStreams)
//  	innerRoundRobbin
//  } 
  
  override lazy val stream = finStreams(minInd).getStream.head #:: innerRoundRobbin.getStream
 
  override def getStream = {
    fine("getStream CareRoundRobbin")
    stream
  }
  
  override def getValues =  {
    fine("getValues LazyRoundRobbin")
    minValue #:: innerRoundRobbin.getValues
  }
  
  override def toString = name
}

object CareRoundRobbin {
  def apply[T](streams: => Seq[Streamable[T]], name: String = "RoundRobbinDef") = {
    new CareRoundRobbin(streams, name: String)
  }
}

//class InitializingRoundRobin[T](val initStreams: List[Streamable[T]])
//	extends LazyStreamable[T] with HasLogger {
//  if (initStreams.isEmpty)
//    println("creating initStreams is empty!!!")
//  
//  var initialized = false
//      
//  var streams: List[Streamable[T]] = List.empty
//    
//  var innerRoundRobbin: RoundRobbin[T] = _
//  
//  override def addStreamable(s: Streamable[T]) = streams :+= s
//  
//  override def isInfinite = 
//    if (initialized) initStreams.exists( _.isInfinite )//innerRoundRobbin.isInfinite
//    else false
//      
//  private def getMinIndex = {
//    
//		val valueIterators = initStreams map { _.getValues.iterator.buffered }
//    
//    var min = Int.MaxValue
//    var minInd = -1
//    var ind = 0
//    while (ind < valueIterators.size) {
//      val indToCheck = ind % valueIterators.size
//      
//      if (valueIterators(indToCheck).hasNext && valueIterators(indToCheck).head < min) {
//      	min = valueIterators(indToCheck).head
//  			minInd = indToCheck
//      }        
//        
//      ind += 1
//    }
//    
//    //assert(minInd > -1, "minInd > -1")
//    (min, minInd)
//  }
//  
//  lazy val (minValue, minInd) = getMinIndex
//  
//  lazy val mappedInitStreams = initStreams.zipWithIndex map {
//    p =>
//    	if (p._2 == minInd)
//    	  SingleStream((p._1.getStream zip p._1.getValues).tail, p._1.isInfinite)
//  	  else p._1
//  }
//    
//  private def produceRoundRobbin = {
//    if (innerRoundRobbin == null)
//    	innerRoundRobbin = RoundRobbin[T]((mappedInitStreams ++ streams).toSeq)
//  	innerRoundRobbin
//  } 
//  
//  override def initialize = {    
//    // important to first initialize
//    // NOT??
//    //produceRoundRobbin
//    initialized = true
//  }
//    
//  lazy val stream = 
//    if (minInd > -1) initStreams(minInd).getStream.head #:: produceRoundRobbin.getStream
//    else Stream.empty    
//  
//  override def getStream = {
//    entering(this.getClass.getName + getName, "getStream")
//    info("initialized " + initialized)
//    
//    if (initialized) stream
//    else Stream.Empty
//  }
//  
//  override def getValues = 
//    if (initialized && minInd > -1) {
//      assert(minInd > -1)
//      minValue #:: produceRoundRobbin.getValues
//    }
//    else Stream.Empty
//}
//
//object InitializingRoundRobin {
//	def apply[T](initStreams: List[Streamable[T]]) = new InitializingRoundRobin(initStreams)
//}