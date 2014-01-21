package insynth.streams.ordered

import scala.collection.mutable

import insynth.streams._

import insynth.util.logging.HasLogger

/**
 * LazyRoundRobbin allows adding Streamables after creation. Added streamables may
 * form recursive links in the structure of the constraint graph. That is why
 * elements from the added streamables are enumerated always after at least one
 * element from initStreams. 
 * NOTE: all addded streamables are considered to be infinite
 */
class LazyRoundRobbin[T](val initStreams: Seq[IntegerWeightStreamable[T]])
	extends IntegerWeightStreamable[T] with AddStreamable[T] with HasLogger {
  require(initStreams.size > 0)
    
  override def getValuedStream = {
    // set of iterators checked for next value
    val initIterators = Array(initStreams.map(_.getValuedStream.iterator.buffered): _*)

    val (minIt, minInd) = initIterators.zipWithIndex.minBy(_._1.head._2)
    
    lazy val allIterators = Array(
      (initIterators ++
      addedStreams.map( _.getValuedStream.iterator.buffered ))
    : _*)
    
    initialized = true

    minIt.head #:: getNext(minInd, allIterators)
  }

  private def getNext(lastIndex: Int, allIterators: Array[BufferedIterator[IntegerWeightPair[T]]]):
    Stream[IntegerWeightPair[T]] = {
    entering("getNext", lastIndex)
    
    allIterators(lastIndex).next
        
    // at the end -1 means no next element was found
    var minInd = -1
    var minValue = Int.MaxValue
    var minStream = Stream[IntegerWeightPair[T]]()
    
    fine("allIterators: " + allIterators.zipWithIndex.mkString(", "))
    // check all iterators by going from first next after previously forwarded
    for (
      ind <- 1 to allIterators.size;
      indToCheck = (lastIndex + ind) % allIterators.size;
      iterator = allIterators(indToCheck);
      if iterator.hasNext
    ) {

      finest("Checking iterator index %d".format(indToCheck))
      if ( iterator.head._2 < minValue ) {
        finest("Iterator ind %d, with head %d is smaller than minValue %d.".format(indToCheck, iterator.head._2, minValue))
        minInd = indToCheck
        minValue = iterator.head._2
        minStream = iterator.head #:: {
          getNext(minInd, allIterators)
        }
      }
      
    }

    exiting("getMinIterator", minStream)
  }
    
  var initialized = false
      
  var filterStreams = mutable.LinkedList[Counted[T]]()
  
  var addedStreams = mutable.LinkedList[IntegerWeightStreamable[T]]()
  
  override def getStreamables = (initStreams ++ addedStreams).toList
    
  // XXX terrible hack, since adding non-ordered streamable will break the code
  override def addStreamable[U >: T](s: Streamable[U]) =
    if (initialized) throw new UnsupportedOperationException("Cannot add new streamables once initialized")
    else addedStreams :+= (s.asInstanceOf[IntegerWeightStreamable[T]])
  
  override def addStreamable[U >: T](s: Traversable[Streamable[U]]) =
    if (initialized) throw new UnsupportedOperationException("Cannot add new streamables once initialized")
    else addedStreams ++= (s.asInstanceOf[Traversable[IntegerWeightStreamable[T]]])
  
  override def isInitialized = initialized
  
  def initialize = { }

  override def size = -1
  
  class RecursiveStreamableIterator(streamable: Counted[T]) extends BufferedIterator[IntegerWeightPair[T]]{
    
    var nextToEnumerate = 0
    
    lazy val iterator = {
      throw new RuntimeException
      streamable.getValuedStream.iterator.buffered
    }
    
    override def hasNext = nextToEnumerate < streamable.enumerated
    
    override def head = iterator.head
    
    override def next = {
      nextToEnumerate += 1
      iterator.next
    }
    
  }

}

object LazyRoundRobbin {
	def apply[T](initStreams: Seq[IntegerWeightStreamable[T]]) =
	  new LazyRoundRobbin(initStreams)
	
	def counted[T](initStreams: Seq[IntegerWeightStreamable[T]]) =
    new LazyRoundRobbin(initStreams) with Counted[T]
}
