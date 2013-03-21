package insynth.reconstruction

import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import insynth.leon.{ LeonQueryBuilder => QueryBuilder, _ }
import insynth.reconstruction.intermediate._

import leon.purescala.Definitions.{ FunDef, VarDecl, Program, ObjectDef }
import leon.purescala.Common.{ FreshIdentifier }
import leon.purescala.TypeTrees._
import leon.purescala.Trees.{ Variable => _, _ }

import insynth.testutil.{ CommonIntermediate, CommonProofTrees }

class IntermediateTransformerTest {

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