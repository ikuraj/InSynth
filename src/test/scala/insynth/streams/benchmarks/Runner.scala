package insynth
package streams.benchmarks

import org.scalatest._

import com.google.caliper.Benchmark
import com.google.caliper.runner.{ CaliperMain => CaliperRunner }

class Runner extends FunSuite {
  
  val benchmarks = Array(
    classOf[BinarySearchTree]
  ).map(_.getName)
  
  println(benchmarks.mkString)

  // main method for IDEs, from the CLI you can also run the caliper Runner directly
  // or simply use SBTs "run" action
  test("Run micro-benchmarks") {
    // we simply pass in the CLI args,
    // we could of course also just pass hardcoded arguments to the caliper Runner
    CaliperRunner.main(classOf[BinarySearchTree], Array())
  }
  
}