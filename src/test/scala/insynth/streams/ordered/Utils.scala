package insynth.streams.ordered

import insynth.streams._

object Utils {
  
  // dummy here for these pesky erasure errors
  def getSingleStream[T](streamToUse: => Stream[(T, Int)], flag: Boolean = false)(implicit s:DummyImplicit): IntegerWeightStreamable[T] =
    WrapperStream(streamToUse, flag)
  
  def getSingleStream(streamToUse: => Stream[Int], flag: Boolean): IntegerWeightStreamable[Int]  =
    getSingleStream(streamToUse zip streamToUse, flag)
  
  def getSingleStream(list: List[Int]): IntegerWeightStreamable[Int] =
  	getSingleStream(list.sortWith(_<_).toStream, false)
  
  def getSingleStream(el: Int): IntegerWeightStreamable[Int] =
  	getSingleStream(Stream(el), false)

  def streamToString[A](stream: Stream[A])(n: Int) = stream.take(n).toList mkString (", ")
  	
}