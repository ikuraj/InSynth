package insynth.streams.ordered

object Utils {
  
  // dummy here for these pesky erasure errors
  def getSingleStream[T](streamToUse: => Stream[(T, Int)], flag: Boolean = false)(implicit s:DummyImplicit): SingleStream[T] =
    TestSingleStream(streamToUse, flag)
  
  def getSingleStream(streamToUse: => Stream[Int], flag: Boolean): SingleStream[Int] =
    TestSingleStream(streamToUse zip streamToUse, flag)

  def getSingleStream[T](streamToUse: => Stream[(T, Int)], size: Int)(implicit s:DummyImplicit): SingleStream[T] =
    TestSingleStreamFixedSize(streamToUse map { _._1 }, streamToUse map { _._2 }, size)
  
  def getSingleStream[T](elements: => Stream[Int], values: => Stream[Int], size: Int): SingleStream[Int] =
    TestSingleStreamFixedSize(elements, values, size)
  
  def getSingleStream(list: List[Int]): SingleStream[Int] =
  	getSingleStream(list.sortWith(_<_).toStream, false)
  
  def getSingleStream(el: Int): SingleStream[Int] =
  	getSingleStream(Stream(el), false)

  def streamToString[A](stream: Stream[A])(n: Int) = stream.take(n).toList mkString (", ")
  	
}