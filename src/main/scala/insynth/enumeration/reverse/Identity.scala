//package insynth.enumeration
//package reverse
//
//import insynth.{ enumeration => e }
//
//trait Identity extends Reverse[Int] {
//  
//  override def reverse(ind: Int) = {
//    ind
//  }
//  
//}
//
//class WrapRange(range: Range) extends e.WrapRange(range) with Reverse[Int] {
//  
//  override def reverse(el: Int) = {
//    el - range.start
//  }
//  
//}