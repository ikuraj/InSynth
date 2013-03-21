package insynth.util.streams.ordered

object Utils {
  
  def getSingleStream(streamToUse: => Stream[Int], flag: Boolean): SingleStream[Int] =
    SingleStream(streamToUse zip streamToUse, flag)
  
  def getSingleStream(list: List[Int]): SingleStream[Int] =
  	getSingleStream(list.sortWith(_<_).toStream, false)
  
  def getSingleStream(el: Int): SingleStream[Int] =
  	getSingleStream(Stream(el), false)

  def streamToString[A](stream: Stream[A])(n: Int) = stream.take(n).toList mkString (", ")
  	
}