package insynth
package enumeration
package benchmarks

import org.scalatest._
import org.scalameter.api._

import dependent._
import memoization._
import insynth.{ enumeration => e }
import insynth.util._

import insynth.util.logging._

trait StructuresBenchmark[DepEnumType] extends DependentMemoizedBenchmark[Int, DepEnumType] {

  override def generator: Gen[Int] =
    Gen.range("size")(1, this.maxSize, 1)
//    Gen.range("size")(this.maxSize, this.maxSize, 1)
    
}
