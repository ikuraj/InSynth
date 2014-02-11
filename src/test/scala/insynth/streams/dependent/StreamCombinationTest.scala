package insynth
package streams
package dependent

import org.scalatest._
import org.scalatest.matchers._

import streams.{ light => e }

import util._
import util.format._
import common._

class StreamCombinationTest extends FunSuite with Matchers {
  
  import Checks._

//  test("sorted lists") {
//    
//    val maxLength = 10
//    
//    val intProducer = Producer.finite(
//      { (v: Int) => e.Singleton(v) }
//    )
//    
//    var getList: Int => e.Finite[List[Int]] = null
//    
//    val listChooser: FiniteDependent[Int, List[Int]] = Producer.finite(
//       { (v: Int) =>
//        v match {
//          case 0 => e.Singleton(List[Int]())
//          case _ => e.Binary( e.Singleton(v), getList(v - 1) )( _ :: _ )
//        }
//       }
//    )
//    
//    getList = (v: Int) => listChooser.getStream(v)
//    
//    val enum = listChooser.getStream(5)
//    
//    enum.size should be (1)
//  }

  test("funny lists") {
    
    val maxLength = 10
    
    val endElementProducer = Producer[(List[Int], Range), List[Int]](
      (pair: (List[Int], Range)) => {
        val (list, range) = pair
        val sum = list.sum
        if (sum > range.end) e.Empty
        else e.WrapperArray( sum to range.end map { _ :: list } )
      }
    )

    var getList: ( (Int, Range) ) => e.Enumerable[List[Int]] = null
    
    val listDep: Dependent[ (Int, Range), List[Int] ] = Producer(
      ( pair: (Int, Range) ) => {
        val (size, range) = pair
        if (size <= 0) e.Singleton( List() )
        else if (size == 1) e.WrapperArray( range map { List(_) } )
        else {
          val smallerList = getList( (size-1, range) )
          BinaryFinite.chain(smallerList, endElementProducer)( (x: List[Int]) => (x, range) )
        }
      }
    )
    
    getList = (v: (Int, Range)) => listDep.getStream(v)
    
    var res: light.Enumerable[List[Int]] = null
    def clue = (0 until res.size).map(res(_)).mkString(",") 
    
    withLazyClue("Elements are: " + clue) {
      res = listDep.getStream((0, 1 to 3))
      res(0) should be (Nil)
      res.size should be (1)
      
      res = listDep.getStream((1, 1 to 3))
      res.size should be (3)
      (0 until res.size).map(res(_)) should contain allOf ( List(1), List(2), List(3) )
      
      (1 to 3).end should be (3)
      res = listDep.getStream( (3, (1 to 3)) )
      res.size should be (3)
      var elements = (0 until res.size) map { res(_) }
      elements should contain allOf (List(2, 1, 1), List(3, 1, 1), List(3, 2, 1))

      res = listDep.getStream( (4, (1 to 5)) )
      res.size should be (3)
      elements = (0 until res.size) map { res(_) }
      elements should contain allOf (
        List(4, 2, 1, 1), List(5, 2, 1, 1),
        List(5, 3, 1, 1)
      )

      res = listDep.getStream( (5, (1 to 10)) )
      elements = (0 until res.size) map { res(_) }
      elements should contain only (
        List(8, 4, 2, 1, 1), List(9, 5, 2, 1, 1), List(10, 5, 2, 1, 1),
        List(10, 6, 2, 1, 1), List(10, 5, 3, 1, 1),
        List(9, 4, 2, 1, 1), List(10, 4, 2, 1, 1)
      )
    }
  }
  
}