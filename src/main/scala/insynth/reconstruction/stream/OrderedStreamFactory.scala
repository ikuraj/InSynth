package insynth.reconstruction.stream

import insynth.util.streams.Streamable
import insynth.util.streams.ordered._

import insynth.util.logging.HasLogger

class OrderedStreamFactory[T] extends StreamFactory[T] with HasLogger {
  
  override def makeEmptyStreamable = Empty

  override def makeSingleton[U <: T](element: U) = Singleton(element)
  
  override def makeSingletonList[U <: T](element: List[U]) = Singleton(element)
  
//  override def makeSingleStream[U <: T](stream: => Stream[U], isInfiniteFlag: Boolean) =
//    SingleStream(stream, isInfiniteFlag)
  
  override def makeUnaryStream[X, Y <: T](streamable: Streamable[X], modify: X=>Y, modifyVal: Option[Int => Int] = None) =
    UnaryStream(streamable.asInstanceOf[OrderedSizeStreamable[X]], modify, modifyVal)
  
  override def makeUnaryStreamList[X, Y <: T](streamable: Streamable[X], modify: X => List[Y]) =
    UnaryStream(streamable.asInstanceOf[OrderedSizeStreamable[X]], modify)
  
  override def makeBinaryStream[X, Y, Z <: T](s1: Streamable[X], s2: Streamable[Y])(combine: (X, Y) => List[Z]) =
    BinaryStream(s1.asInstanceOf[OrderedSizeStreamable[X]], s2.asInstanceOf[OrderedSizeStreamable[Y]])(combine)
  
  override def makeRoundRobbin[U <: T](streams: Seq[Streamable[U]]) =
    RoundRobbin(streams.asInstanceOf[Seq[OrderedSizeStreamable[U]]])
  
  override def makeLazyRoundRobbin[U <: T](initStreams: List[Streamable[U]]) =
    LazyRoundRobbin[U](initStreams.asInstanceOf[List[OrderedSizeStreamable[U]]])
      
  def getFinalStream(streamable: Streamable[T]) = 
    streamable match {
      case os: OrderedSizeStreamable[_] =>
        fine("returning ordered streamable")
        os.getStream zip os.getValues.map(_.toFloat)
      case _: Streamable[_] =>
        fine("returning unordered streamable")
        streamable.getStream zip Stream.continually(0f)
    }
}