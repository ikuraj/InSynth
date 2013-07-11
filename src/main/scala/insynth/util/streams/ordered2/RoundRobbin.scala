package insynth.util.streams.ordered2

import scala.collection.mutable.{ Seq => MutableSeq }

import insynth.util.logging.HasLogger
import insynth.util.streams.ordered2.{ OrderedSizeStreamable => Streamable }
import insynth.util.streams.unordered.{ RoundRobbin => UnRoundRobbin }

class RoundRobbin[T] protected[streams] (val streams: Seq[Streamable[T]], name: String)
	extends Streamable[T] with HasLogger {
    
  override def depleted: Boolean = getNextIndex._2 == -1 // wtv
  override def nextReady: Boolean = streams.exists(! _.nextReady)
  
  // iterators that track the positions in each stream
  // hidden so it looks as functional
  private var _valueIterators: Array[BufferedIterator[Int]] = Array.fill(streams.size)(null)
  protected var _iterators: Array[Iterator[T]] = Array.fill(streams.size)(null)
   
  protected def iterators(ind: Int) = {
    if (_iterators(ind) == null) {
      _iterators(ind) = streams(ind).getStream.iterator
    }
    _iterators(ind)
  }
  protected def valueIterators(ind: Int) = {
    if (_valueIterators(ind) == null) {
      _valueIterators(ind) = streams(ind).getValues.iterator.buffered 
    }
    _valueIterators(ind)
  }
    
  val valueIteratorsSize = streams.size
  
  var currentInd = 0
  
  // NOTE keeps fairness
  private def getNextIndex = {
    //entering("getNextIndex", currentInd.toString)
    var min = Int.MaxValue
    var minInd = -1
    var ind = 0
    while (ind < valueIteratorsSize) {
      val indToCheck = (currentInd + ind) % valueIteratorsSize
      
      if (streams(indToCheck).nextReady) {
	      fine("checking index: " + indToCheck + ", valueIterators(indToCheck).hasNext: " + valueIterators(indToCheck).hasNext)
	      if (valueIterators(indToCheck).hasNext) fine("valueIterators(indToCheck).head: " + valueIterators(indToCheck).head)
	      if (valueIterators(indToCheck).hasNext && valueIterators(indToCheck).head < min) {
	      	fine("index passed: " + indToCheck + ", value: " + valueIterators(indToCheck).head)
	      	min = valueIterators(indToCheck).head
	  			minInd = indToCheck
	      }        
      }
        
      ind += 1
    }
    
    exiting("getNextIndex", (min, minInd).toString)
    (min, minInd)
  }
  
  // stream value, store it in order to use memoization of previously computed elements
  protected lazy val streamWithValues = { 
    // inner function which "produces" new elements
    // TODO wow a bug found due to the name!?
    def loopXXX(index: Int): Stream[(T, Int)] = {
  		//entering(this.getClass.getName, "loop:" + this.getName, index)
  		
      currentInd = index
      // check if there is at least one iterator with next element
  		getNextIndex match {
  		  case (nextValue, nextIndex) if nextIndex > -1 =>
  		    // forward the value iterator
  		    valueIterators(nextIndex).next
			    // prepend the element to a recursively computed stream
			  	(iterators(nextIndex).next, nextValue) #:: loopXXX((index + 1) % streams.size)
  		  case _ =>
				  // no iterator has next, return empty stream
				  Stream.empty  		    
  		}
    }
			  
	  // start with first iterator
    loopXXX(0)
  }
  
  override def getStream = {
    fine("getStream RoundRobbin")
    streamWithValues map { _._1 }
  }
  
  override def getValues = {
    fine("getValues LazyRoundRobbin")
    streamWithValues map { _._2 }
  }
  
  override def toString = name
}

object RoundRobbin {
  def apply[T](streamsIn: => Seq[Streamable[T]], name: String = "RoundRobDef") = {
//    assert(streams.forall(!_.isInfinite))
//  	val streams = streamsIn.sortWith(!_.isInfinite && _.isInfinite)
  	
    new RoundRobbin(streamsIn, name: String)
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