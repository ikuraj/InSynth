package insynth.enumeration
package dependent

import scala.collection.immutable.{ Map => ScalaMap }
import scala.reflect._
import scala.language.higherKinds

trait Depend[I, +O] extends Serializable {
  
  type EnumType <: Enum[O]

  def apply(parameter: I) = getEnum(parameter)
  
  def getEnum(parameter: I): EnumType
  
  // concatenation
  def concat[O2](e: Depend[I, O2]) =
    Concat(this, e)
    
  def ++[O2](e: Depend[I, O2]) = concat(e)
  def ⊕[O2](e: Depend[I, O2]) = concat(e)

  // products
  def product[O2](e: Depend[I, O2]): Depend[I, (O, O2)] =
    Product(this, e)
    
  def **[O2](e: Depend[I, O2]) = product(e)
  def ⊗[O2](e: Depend[I, O2]) = product(e)
  
  // chain
  def chain(e: Enum[I]): Enum[(I, O)] =
    Chain(e, this)
    
  def ⊘(e: Enum[I]) = chain(e)
  
  // inmap
  def inmap[I2](f: PartialFunction[I2, I]) = {
    InMapP(this, f)
  }

  def ↓[I2](f: PartialFunction[I2, I]) = inmap(f)
  
//  // is this OK?
//  def inmap(f: PartialFunction[Any, I]) = {
//    InMapP(this, f)
//  }
//
//  def ↓(f: PartialFunction[Any, I]) = inmap(f)
  
}

object Depend {
  
  def apply[I, O, E <: Enum[O]](producerFunction: (Depend[I, O], I) => E) =
    new WrapFunction(producerFunction)
  
  def rec[I, O, E <: Enum[O]](producerFunction: (Depend[I, O], I) => E) =
    new WrapFunction(producerFunction)
  
  def apply[I, O, F[O] <: Enum[O]](producerFunction: I => F[O])
  	(implicit ct: ClassTag[F[_]]): Depend[I, O] = {
    val finiteTag = implicitly[ClassTag[Finite[_]]]
    val infiniteTag = implicitly[ClassTag[Infinite[_]]]
    implicitly[ClassTag[F[_]]] match {
      case `finiteTag` =>
        val fun = producerFunction.asInstanceOf[I => Finite[O]]
      	new WrapFunction[I, O, Finite[O]](fun) with DependFinite[I, O]
      case _: Infinite[_] =>
        val fun = producerFunction.asInstanceOf[I => Infinite[O]]
      	new WrapFunction[I, O, Infinite[O]](fun) with DependInfinite[I, O]
      case _ =>
        new WrapFunction[I, O, F[O]](producerFunction)
    }
  }
  
  def fin[I, O](fun: I => Finite[O]): DependFinite[I, O] = {
    new WrapFunction[I, O, Finite[O]](fun) with DependFinite[I, O]
  }
    
  def fin[I, O](fun: (DependFinite[I, O], I) => Finite[O]): DependFinite[I, O] = {    
    new WrapFunctionFin[I, O](fun) with DependFinite[I, O]
  }
  
//  def apply[I, O](producerFunction: I => Finite[O]) =
//    new WrapFunction[I, O, Finite[O]](producerFunction) with DependFinite[I, O]
//  
//  def apply[I, O](producerFunction: I => Infinite[O]) =
//    new WrapFunction[I, O, Infinite[O]](producerFunction) with DependInfinite[I, O]
//  
//  def apply[I, O, E <: Enum[O]](producerMap: ScalaMap[I, E]) =
//    new WrapMap[I, O, E](producerMap)
  
  def map[I, O, E <: Enum[O]](producerMap: ScalaMap[I, E] = ScalaMap.empty) =
    new WrapMap[I, O, E](producerMap)
  
  import memoization.MemoizationScope
  import memoization.dependent._
    
  def memoized[I, O, F[O] <: Enum[O]](producerFunction: I => F[O])
    (implicit ct: ClassTag[F[_]], ms: MemoizationScope = null): Depend[I, O] = {
    val finiteTag = implicitly[ClassTag[Finite[_]]]
    val infiniteTag = implicitly[ClassTag[Infinite[_]]]
    val enum =
      implicitly[ClassTag[F[_]]] match {
        case `finiteTag` =>
          val fun = producerFunction.asInstanceOf[I => Finite[O]]
          new WrapFunction[I, O, Finite[O]](fun) with DependFinite[I, O] with Memoized[I, O]
        case _: Infinite[_] =>
          val fun = producerFunction.asInstanceOf[I => Infinite[O]]
          new WrapFunction[I, O, Infinite[O]](fun) with DependInfinite[I, O] with Memoized[I, O]
        case _ =>
          new WrapFunction[I, O, F[O]](producerFunction) with Memoized[I, O]
      }
        
    if (ms != null) ms add enum
    enum
  }
    
  def memoized[I, O, F[O] <: Enum[O]](producerFunction: (Depend[I, O], I) => F[O])
    (implicit ct: ClassTag[F[_]], ms: MemoizationScope = null): Depend[I, O] = {
    val finiteTag = implicitly[ClassTag[Finite[_]]]
    val infiniteTag = implicitly[ClassTag[Infinite[_]]]
    val enum =
      implicitly[ClassTag[F[_]]] match {
        case `finiteTag` =>
          val fun = producerFunction.asInstanceOf[(Depend[I, O], I) => Finite[O]]
          new WrapFunction[I, O, Finite[O]](fun) with DependFinite[I, O] with Memoized[I, O]
        case _: Infinite[_] =>
          val fun = producerFunction.asInstanceOf[(Depend[I, O], I) => Infinite[O]]
          new WrapFunction[I, O, Infinite[O]](fun) with DependInfinite[I, O] with Memoized[I, O]
        case _ =>
          new WrapFunction[I, O, F[O]](producerFunction) with Memoized[I, O]
      }
        
    if (ms != null) ms add enum
    enum
  }
    
  def memoizedFin[I, O](fun: (DependFinite[I, O], I) => Finite[O])
    (implicit ms: MemoizationScope = null): DependFinite[I, O] = {
    val enum =
      new WrapFunctionFin[I, O](fun) with DependFinite[I, O] with Memoized[I, O]
        
    if (ms != null) ms add enum
    enum
  }
    
  def memoizedFin[I, O](fun: I => Finite[O])
    (implicit ms: MemoizationScope = null): DependFinite[I, O] = {
    val enum =
      new WrapFunctionFin[I, O](fun) with DependFinite[I, O] with Memoized[I, O]
        
    if (ms != null) ms add enum
    enum
  }
  
  case class FunctionToInMapWrapper[I, I2](f: I => I2) {
    def ↓[O](d: Depend[I2, O]) = InMap(d, f)
  }
  implicit def functionToInMapWrapper[I, I2](f: I => I2) =
    FunctionToInMapWrapper(f)
  
  case class PartialFunctionToInMapWrapper[I, I2](f: PartialFunction[I, I2]) {
    def ↓[O](d: Depend[I2, O]) = InMapP(d, f)
  }
  implicit def partialFunctionToInMapWrapper[I, I2](f: PartialFunction[I, I2]) =
    PartialFunctionToInMapWrapper(f)
    
  def rec[I, O](producerFunction: PartialFunction[(Depend[I, O], I), Enum[O]])
    ( implicit
//        ct: ClassTag[F[_]],
      ms: MemoizationScope = null): Depend[I, O] = {
    val enum =
    {
//        new WrapFunctionP[I, O, Finite[O]](producerFunction) with DependFinite[I, O] with Memoized[I, O]
        new WrapFunctionP[I, O, Enum[O]](producerFunction) with Memoized[I, O]
//    val finiteTag = implicitly[ClassTag[Finite[_]]]
//    val infiniteTag = implicitly[ClassTag[Infinite[_]]]
//      implicitly[ClassTag[F[_]]] match {
//        case `finiteTag` =>
//          val fun = producerFunction.asInstanceOf[(Depend[I, O], I) => Finite[O]]
//          new WrapFunction[I, O, Finite[O]](fun) with DependFinite[I, O] with Memoized[I, O]
//        case _: Infinite[_] =>
//          val fun = producerFunction.asInstanceOf[(Depend[I, O], I) => Infinite[O]]
//          new WrapFunction[I, O, Infinite[O]](fun) with DependInfinite[I, O] with Memoized[I, O]
//        case _ =>
//          new WrapFunction[I, O, F[O]](producerFunction) with Memoized[I, O]
      }
        
    if (ms != null) ms add enum
    enum
  }

  def memoizedP[I, O](producerFunction: PartialFunction[(Depend[I, O], I), Enum[O]])
    ( implicit
//        ct: ClassTag[F[_]],
      ms: MemoizationScope = null): Depend[I, O] = {
    val enum =
    {
//        new WrapFunctionP[I, O, Finite[O]](producerFunction) with DependFinite[I, O] with Memoized[I, O]
        new WrapFunctionP[I, O, Enum[O]](producerFunction) with Memoized[I, O]
//    val finiteTag = implicitly[ClassTag[Finite[_]]]
//    val infiniteTag = implicitly[ClassTag[Infinite[_]]]
//      implicitly[ClassTag[F[_]]] match {
//        case `finiteTag` =>
//          val fun = producerFunction.asInstanceOf[(Depend[I, O], I) => Finite[O]]
//          new WrapFunction[I, O, Finite[O]](fun) with DependFinite[I, O] with Memoized[I, O]
//        case _: Infinite[_] =>
//          val fun = producerFunction.asInstanceOf[(Depend[I, O], I) => Infinite[O]]
//          new WrapFunction[I, O, Infinite[O]](fun) with DependInfinite[I, O] with Memoized[I, O]
//        case _ =>
//          new WrapFunction[I, O, F[O]](producerFunction) with Memoized[I, O]
      }
        
    if (ms != null) ms add enum
    enum
  }
    
}