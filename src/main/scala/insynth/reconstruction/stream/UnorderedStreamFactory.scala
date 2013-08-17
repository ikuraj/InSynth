package insynth.reconstruction.stream

import insynth.util.streams.Streamable
import insynth.util.streams.unordered._

class UnorderedStreamFactory[T] extends StreamFactory[T] {
  
  override def makeEmptyStreamable = Empty

  override def makeSingleton[U <: T](element: U) = Singleton(element)
  
  override def makeSingletonList[U <: T](element: List[U]) = Singleton(element)
  
//  override def makeSingleStream[U <: T](stream: => Stream[U], isInfiniteFlag: Boolean) =
//    SingleStream(stream, isInfiniteFlag)
  
  override def makeUnaryStream[X, Y <: T](streamable: Streamable[X], modify: X=>Y, modifyVal: Option[Int => Int] = None) =
    UnaryStream(streamable, modify)
  
  override def makeUnaryStreamList[X, Y <: T](streamable: Streamable[X], modify: X => List[Y]) =
    UnaryStream(streamable, modify)
  
  override def makeBinaryStream[X, Y, Z <: T](s1: Streamable[X], s2: Streamable[Y])(combine: (X, Y) => List[Z]) =
    BinaryStream(s1, s2)(combine)
  
  override def makeRoundRobbin[U <: T](streams: Seq[Streamable[U]]) =
    RoundRobbin(streams)
  
  override def makeLazyRoundRobbin[U <: T](initStreams: List[Streamable[U]]) =
    LazyRoundRobbin(initStreams)
        
  def getFinalStream(streamable: Streamable[T]) =
    streamable.getStream zip Stream.continually(0f)
  
}