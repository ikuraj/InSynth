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
    val minIt = initIterators.minBy(_.head._2)
    
    initialized = true

    minIt.next #:: getNext(0)
  }

  private def getNext(lastIndex: Int): Stream[IntegerWeightPair[T]] = {
    entering("getNext", lastIndex)
        
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
          getNext(minInd)
        }
      }
      
    }
    
    allIterators(minInd).next

    exiting("getMinIterator", minStream)
  }
  
  // set of iterators checked for next value
  val initIterators = Array(initStreams.map(_.getValuedStream.iterator.buffered): _*)
  
  lazy val allIterators = Array(
    (initIterators ++
    addedStreams.map(_.getValuedStream.iterator.buffered))
  : _*)
    
  var initialized = false
      
  var addedStreams = mutable.LinkedList[Counted[T]]()
  
  override def getStreamables = (initStreams ++ addedStreams).toList
    
  // XXX terrible hack, since adding non-ordered streamable will break the code
  override def addStreamable[U >: T](s: Streamable[U]) =
    addedStreams :+= (s.asInstanceOf[Counted[T]])
  
  override def addStreamable[U >: T](s: Traversable[Streamable[U]]) =
    addedStreams ++= (s.asInstanceOf[Traversable[Counted[T]]])
  
  override def isInitialized = initialized
  
  def initialize = { }

  override def size = -1

}

object LazyRoundRobbin {
	def apply[T](initStreams: Seq[IntegerWeightStreamable[T]]) =
	  new LazyRoundRobbin(initStreams)
	
	def counted[T](initStreams: Seq[IntegerWeightStreamable[T]]) =
    new LazyRoundRobbin(initStreams) with Counted[T]
}
