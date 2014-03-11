package insynth.streams
package dependent

import light._

trait Dependent[I, +O] {

  def getStream(parameter: I): Enumerable[O]
  
}

// should return all finite/infinite enumerables

//trait FiniteDependent[I, +O] extends Dependent[I, O] {
//
//  override def getStream(parameter: I): Finite[O]
//  
//}
//
//trait InfiniteDependent[I, +O] extends Dependent[I, O] {
//
//  override def getStream(parameter: I): Infinite[O]
//  
//}