package insynth.enumeration
package lzy

import combinators.Product

import _root_.insynth.util
import util.Math._
import util.logging._

// Cantor inverse mapping is fine in this case
protected[enumeration] class ProductInfinite[T, V]
	(override val left: Infinite[T], override val right: Infinite[V])
	extends Product[T, V] with Infinite[(T, V)] with HasLogger {
  
  override def apply(ind: Int) = {
    val (i1, i2) = cantorInverse(ind)
    ( left(i1), right(i2) )
  }
  
}

// optimization
protected[enumeration] class ProductInfiniteComb[T, V, U]
	(val left: Infinite[T], val right: Infinite[V])
	(combine: (T, V) => U)
	extends Infinite[U] with HasLogger {
  
  override def apply(ind: Int) = {
    val (i1, i2) = cantorInverse(ind)
    combine( left(i1), right(i2) )
  }
  
}