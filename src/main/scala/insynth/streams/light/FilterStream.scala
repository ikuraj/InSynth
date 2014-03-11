package insynth.streams.light

//class FilterStream[T](val streamable: IntegerWeightStreamable[T], val filterFun: T => Boolean)
//	extends IntegerWeightStreamable[T] with Filterable[T] {
//  
//  override def getValuedStream = //streamable.getValuedStream filter { p => filterFun(p._1) }
//  {
//    def loop(it: Iterator[IntegerWeightPair[T]]): Stream[IntegerWeightPair[T]] = {
//      while (it.hasNext) {
//        val el = it.next
//        if (filterFun(el._1)) return el #:: loop(it)
//      }
//      Stream.empty
//    }
//
//    loop(streamable.getValuedStream.iterator)
//  }
//  
////  lazy val st = streamable.getValuedStream filter { p => filterFun(p._1) }
////  
////  override def getValuedStream = st
//  
//  override def size = -1
//  
//}
//
//class FilterStreamCounted[T](val streamable: IntegerWeightStreamable[T], val filterFun: T => Boolean)
//  extends IntegerWeightStreamable[T] with OrderedCountable[T] with Filterable[T] {
//  
//  var enumeratedCount = 0
//  
//  lazy val st = streamable.getValuedStream filter { p =>
//    val res = filterFun(p._1)
//    if (res) enumeratedCount+= 1
//    res
//  }
//  
//  override def enumerated = enumeratedCount
//  
//  override def getValuedStream = st
//  
//  override def size = -1
//  
//}
//
//object FilterStream {
//  def apply[T](streamable: IntegerWeightStreamable[T], filterFun: T => Boolean) =
//    new FilterStream(streamable, filterFun)
//
//  def memoized[T](streamable: IntegerWeightStreamable[T], filterFun: T => Boolean) =
//    new FilterStream(streamable, filterFun) with Memoized[T]
//
//  def countedMemoized[T](streamable: IntegerWeightStreamable[T], filterFun: T => Boolean) =
////    new FilterStreamCounted(streamable, filterFun)
//    new FilterStreamCounted(streamable, filterFun)
//  
//  def memoizedCounted[T](streamable: IntegerWeightStreamable[T], filterFun: T => Boolean) =
////    new FilterStreamCounted(streamable, filterFun)
//    new FilterStreamCounted(streamable, filterFun)
//
//  def counted[T](streamable: IntegerWeightStreamable[T], filterFun: T => Boolean) =
////    new FilterStreamCounted(streamable, filterFun)
//    new FilterStream(streamable, filterFun) with OrderedCounted[T]
//}