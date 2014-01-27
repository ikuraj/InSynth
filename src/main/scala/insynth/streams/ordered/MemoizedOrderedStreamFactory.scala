package insynth.reconstruction.stream

import insynth.streams._
import insynth.streams.ordered._

import insynth.util.logging.HasLogger

class MemoizedOrderedStreamFactory[T] extends OrderedStreamFactory[T] with HasLogger {
  
  override def makeEmptyStreamable = Empty

  override def makeSingleton[U <: T](element: U) = Singleton(element)
  
  override def makeSingletonList[U <: T](element: List[U]) = Singleton(element)
  
  override def makeSingleStream[U <: T](stream: => Stream[(U, Int)]) =    
    WrapperStream(stream)

  override def makeFiniteStream[U <: T](array: => Vector[(U, Int)]) =
    FiniteStream.memoized(array)
  
  override def makeUnaryStream[X, Y <: T](streamable: Streamable[X], modify: X=>Y, modifyVal: Int => Int) =
    UnaryStream.memoized(streamable.asInstanceOf[IntegerWeightStreamable[X]], modify, modifyVal)

  override def makeUnaryStream[X, Y <: T](streamable: Streamable[X], modify: X=>Y) =
    UnaryStream.memoized(streamable.asInstanceOf[IntegerWeightStreamable[X]], modify)
  
  override def makeUnaryStreamList[X, Y <: T](streamable: Streamable[X], modify: X => List[Y]) =
    UnaryStream.memoized(streamable.asInstanceOf[IntegerWeightStreamable[X]], modify)
    
  override def makeFilterStream[U <: T](streamable: Streamable[U], filterFun: U => Boolean) =
    FilterStream.memoized(streamable.asInstanceOf[IntegerWeightStreamable[U]], filterFun)
  
  override def makeBinaryStream[X, Y, Z <: T](s1: Streamable[X], s2: Streamable[Y])(combine: (X, Y) => List[Z]) =
    BinaryStream.memoized(s1.asInstanceOf[IntegerWeightStreamable[X]], s2.asInstanceOf[IntegerWeightStreamable[Y]])(combine)
  
  override def makeRoundRobbin[U <: T](streams: Seq[Streamable[U]]) =
    RoundRobbin.memoized(streams.asInstanceOf[Seq[IntegerWeightStreamable[U]]])
  
  override def makeLazyRoundRobbin[U <: T](initStreams: Seq[Streamable[U]]) =
    LazyRoundRobbin.memoized[U](initStreams.asInstanceOf[Seq[IntegerWeightStreamable[U]]])
      
  override def makeLazyRoundRobbinList[U <: T](initStreams: Seq[Streamable[List[U]]]) =    
    LazyRoundRobbin.memoized[List[U]](initStreams.asInstanceOf[Seq[IntegerWeightStreamable[List[U]]]])
  
  override def getFinalStream(streamable: Streamable[T]) = 
    streamable match {
      case os: IntegerWeightStreamable[_] =>
        fine("returning ordered streamable")
        os.getStream zip os.getValues.map(_.toFloat)
      case _: Streamable[_] =>
        fine("returning unordered streamable")
        streamable.getStream zip Stream.continually(0f)
    }
  
  override def memoized = this
}