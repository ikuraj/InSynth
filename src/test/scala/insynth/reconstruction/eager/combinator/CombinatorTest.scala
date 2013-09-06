package insynth.reconstruction.eager
package combinator

import insynth.{ structures => base }

import insynth.util.format.{ FormatSuccinctNode => FormatNode }

import scala.language.implicitConversions

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.Test

class CombinatorTest extends JUnitSuite {
  
  import PrunedTreeOperations._
  import TreeExample._

  val numberOfCombinations = 15
  val maximumTime = 500
  
  implicit def toFormatNode(sn: base.SimpleNode) = FormatNode(sn)
  implicit def toPrFormatNode(sn: Node) = FormatPrNode(sn)

  def main(args: Array[String]): Unit = {
    val tests =      
      Array(
        TreeExample.buildSimpleTree, TreeExample.buildComplexTree,
        TreeExample.buildTreeAbsApplication, TreeExample.buildTreeArrowType,
          TreeExample.buildTreeOverlapParameterTypeWithReturnType,
        TreeExample.buildTreeSKombinator,
        TreeExample.buildTreeWithVariousFunctions,
        TreeExample.buildTreeIdentityFunction
      )
    
    for (tree <- tests)
      parametrizedCombine(tree) 
    cycleTreeCombine
  }
  
  def parametrizedCombine(sn: base.SimpleNode) = {
    println("original tree")
    FormatNode(sn)
    println("combined tree")
    //FormatPrNode(Combinator(sn, numberOfCombinations, maximumTime))
  } 
  
  // XXX cannot still be instantiated according to the proof representation!
  @Test
  def cycleTreeCombine {
    //println("combined cycle tree")
    val cycleTree = TreeExample.buildTreeCycles
    val result = Combinator(cycleTree, numberOfCombinations, maximumTime)
  } 
  
  @Test
  def simpleTreeCombine {
    //println("simple tree")
    val result = 
      Combinator(TreeExample.buildSimpleTree, 2, maximumTime) match {
      	case Some(x) => x
      	case None =>
      	  fail("Combinator failed")
      	  null
    	}
    assertTrue(        
      checkInhabitants(result,
        StringNode(queryDeclaration.getSimpleName, Set(
          StringNode(m4Declaration.getSimpleName)
      )))
		)
  }
  
  @Test
  def complexTreeCombine {
    //println("complex tree")
    Combinator(TreeExample.buildComplexTree, 2, maximumTime)
  }
  
  @Test
  def arrowTreeCombine {
    //println("arrow tree")
    Combinator(TreeExample.buildTreeArrowType, 6, maximumTime)
  }
  
  @Test
  def overlapTreeCombine {
    //println("overlap tree")
    Combinator(TreeExample.buildTreeOverlapParameterTypeWithReturnType, 6, maximumTime)
  }
    
  @Test
  def sKombinatorTreeReconstruct {
    //println("s combinator tree")
    Combinator(TreeExample.buildTreeSKombinator, 6, maximumTime)
  }
    
  @Test
  def buildTreeIdentityFunction {
    Combinator(TreeExample.buildTreeIdentityFunction, 6, maximumTime)
  }
    
  @Test
  def withVariousFunctions {
    Combinator(TreeExample.buildTreeWithVariousFunctions, 6, maximumTime)
  }
    
  @Test
  def absApplication {
    Combinator(TreeExample.buildTreeAbsApplication, 6, maximumTime)
  }

}