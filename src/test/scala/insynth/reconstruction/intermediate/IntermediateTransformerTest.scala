package insynth.reconstruction.intermediate

import org.scalatest.junit.JUnitSuite

import org.junit.Assert._
import org.junit.Test

import insynth.reconstruction.intermediate._
import insynth.common._

class IntermediateTransformerTest extends JUnitSuite {

  import CommonIntermediate._
  import CommonProofTrees._

  @Test
  def testIntToInt = {
    // construct answer
    val intermediateTree = constructIntToIntIntermediate
    // do the transform
    val result = IntermediateTransformer(exampleIntToInt)
    // compare
    assertEquals(intermediateTree, result)
    assertEquals(intermediateTree.params(1).head.asInstanceOf[Application].recursiveParams,
      result.asInstanceOf[Application].params(1).head.asInstanceOf[Application].recursiveParams)
  }
  
  @Test
  def testFunctionIntToInt = {
    // construct answer
    val intermediateTree = constructIntToIntIntermediateBool
    // do the transform
    val result = IntermediateTransformer(exampleIntToIntBool)
    // compare
    assertEquals(intermediateTree, result)
//    assertEquals(
//      // add first element since (recursiveParams.size == params.size) must hold and params
//      // include application initial term
//      Set() +: intermediateTree.params(1).head.asInstanceOf[Application].recursiveParams,
//      result.asInstanceOf[Application].params(1).head.asInstanceOf[Application].recursiveParams)
  }
  
  @Test
  def testFunctionIntToIntBoth = {
    // construct answer
    val intermediateTree = constructIntToIntIntermediateBoth
    // do the transform
    val result = IntermediateTransformer(exampleIntToIntBoth)
    // compare
    assertEquals(intermediateTree, result)
//    assertEquals(
//      // add first element since (recursiveParams.size == params.size) must hold and params
//      // include application initial term
//      Set() +: intermediateTree.params(1).head.asInstanceOf[Application].recursiveParams,
//      result.asInstanceOf[Application].params(1).head.asInstanceOf[Application].recursiveParams)
  }

}