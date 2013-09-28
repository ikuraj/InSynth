package insynth.reconstruction

import insynth.reconstruction.stream.Application

import org.scalatest.junit.JUnitSuite

import org.junit.{ Test, Ignore, BeforeClass, AfterClass }
import org.junit.Assert._

import insynth.common._

class StreamerTest extends JUnitSuite {

  import CommonDeclarations._
  import CommonProofTrees._
  import CommonUtils._
  import CommonLambda._
  
  val maxNumberToEnumerate = 200
  
  @Test
  def treeBoolToInt {
    val (queryNode, query) = exampleBoolToInt
    
    val expStream = Streamer(queryNode)
    
    assertEquals(1, expStream.size)
    
    val result = expStream.head
    
    assertEquals(constructBooleanToIntIntermediateLambda.head, result._1)
    assertEquals(0f, result._2, 0f)    
  }
  
  
  @Test
  def treeIntToIntBoth {
    val queryNode = exampleIntToIntBoth
    
    val expStream = Streamer(queryNode)
    
    val expressions = expStream.map(
      _._1 match {
        case Application(_, funId :: onlyArg :: Nil) => onlyArg
        case other => other
      }
    ).take(20).toSet
    val message = expressions.mkString("\n")
    
//    assertTrue("Not found: " + boolInv + " in\n" + message, expressions contains boolInv)
    assertTrue("Not found: " + inv1WithBoolInv + " in\n" + message, expressions contains inv1WithBoolInv)
    assertTrue(message, expressions contains inv1WithInt)
    assertTrue(message, expressions contains inv2WithInt)
    assertTrue(message, expressions contains inv3WithInt)  
    assertTrue(message, expressions contains inv2WithBoolInv)    
    assertTrue(message, expressions contains inv3WithBoolInv)      
  }
  
  
  @Test
  def treeIntToIntBothOrdered {
    val queryNode = exampleIntToIntBoth
    
    val expStream = Streamer(queryNode, true)
    
    val expressions = assertTake(expStream, 20).map(
      _._1 match {
        case Application(_, funId :: onlyArg :: Nil) => onlyArg
        case other => other
      }
    )
    
    val listOfExpressions = List(boolInv, inv1WithInt, inv1WithBoolInv, inv2WithInt,
      inv3WithInt, inv2WithBoolInv, inv3WithBoolInv)
    
    for (exp <- listOfExpressions)
    	assertTrue(expressions.toSet contains exp)
    	
  	{
	    val listOfExpressionsOrder = List(boolInv, inv2WithInt,
	      inv2WithBoolInv, inv3WithBoolInv)
	    
	    for (ind <- 0 until listOfExpressionsOrder.size - 1)
	      assertTrue("Expression " + listOfExpressionsOrder(ind) + " (position " + expressions.indexOf(listOfExpressionsOrder(ind)) +
	        ") should occur before expression " + listOfExpressionsOrder(ind+1) + " (position " + expressions.indexOf(listOfExpressionsOrder(ind + 1)) + ")",
	        expressions.indexOf(listOfExpressionsOrder(ind)) < expressions.indexOf(listOfExpressionsOrder(ind+1)))
  	}
    
  	{
	    val listOfExpressionsOrder = List(boolInv, inv1WithBoolInv,
	      inv2WithBoolInv, inv3WithBoolInv)
	    
	    for (ind <- 0 until listOfExpressionsOrder.size - 1)
	      assertTrue("Expression " + listOfExpressionsOrder(ind) + " (position " + expressions.indexOf(listOfExpressionsOrder(ind)) +
	        ") should occur before expression " + listOfExpressionsOrder(ind+1) + " (position " + expressions.indexOf(listOfExpressionsOrder(ind + 1)) + ")",
	        expressions.indexOf(listOfExpressionsOrder(ind)) < expressions.indexOf(listOfExpressionsOrder(ind+1)))
  	}
  }

  @Test
  def testComplexTree {
    import CommonLambda.BuildComplexTree._

    val queryNode = buildComplexTree
    
    val expStream = Streamer(queryNode, true)
    
    val expressions = assertTake(expStream, maxNumberToEnumerate).map(
      _._1 match {
        case Application(_, funId :: onlyArg :: Nil) => onlyArg
        case other => other
      }
    )
        
    for (expectedExp <- lambdaNodes) {
      val isFound = (false /: expressions) {
        (res, exp) => res || compareNodesModuloVariableName(exp, expectedExp)
      }
      
    	assertTrue(expressions.toSet.mkString("\n---\n")  + " do not contain (module variable names) " + expectedExp,
  	    isFound)
    }
  }

  
  @Test
  def testLigtherComplexTree = {    
    import CommonLambda.BuildLighterComplexTree._
    
    val queryNode = buildLighterComplexTree
    val expStream = Streamer(queryNode, true)
    
    val expressions = assertTake(expStream, maxNumberToEnumerate).map(
      _._1 match {
        case Application(_, funId :: onlyArg :: Nil) => onlyArg
        case other => other
      }
    )
    
    for (exp <- lambdaNodes)
    	assertTrue(expressions.toSet.mkString("\n---\n") + " do not contain " + exp, expressions.toSet contains exp)
  }

  @Test
  def testMultipleVarTree = {    
    import CommonLambda.BuildMultipleVarTree._
    
    val queryNode = buildMultipleVarTree
    val expStream = Streamer(queryNode, true)
    
    val expressions = assertTake(expStream, maxNumberToEnumerate).map(
      _._1 match {
        case Application(_, funId :: onlyArg :: Nil) => onlyArg
        case other => other
      }
    )
    assertEquals(2, expressions.size)
    
    for (expectedExp <- expressions) {
      val isFound = (false /: lambdaNodes) {
        (res, exp) => res || compareNodesModuloVariableName(exp, expectedExp)
      }
      
    	assertTrue(lambdaNodes.toSet.mkString("\n---\n")  + " do not contain (module variable names) " + expectedExp, isFound)
    }
  }

  @Test
  def testArrowTypeTree = {    
    import CommonLambda.BuildArrowTypeTree._
    
    val queryNode = buildArrowTypeTree
    val expStream = Streamer(queryNode, true)
    
    val expressions = assertTake(expStream, maxNumberToEnumerate).map(
      _._1 match {
        case Application(_, funId :: onlyArg :: Nil) => onlyArg
        case other => other
      }
    )
    assertEquals(expressions.mkString("\n\n"), 9, expressions.size)
    
    for (expectedExp <- lambdaNodes) {
      val isFound = (false /: expressions) {
        (res, exp) => res || compareNodesModuloVariableName(exp, expectedExp)
      }
      
    	assertTrue(expressions.mkString("\n---\n")  + " do not contain (module variable names) " + expectedExp,
  	    isFound)
    }
  }

  @Test
  def testArrowTypeTreeMoreComplex = {    
    import CommonLambda.BuildArrowTypeTreeMoreComplex._
    
    val queryNode = buildArrowTypeTreeMoreComplex
    val expStream = Streamer(queryNode, true)
    
    val expressions = assertTake(expStream, maxNumberToEnumerate).map(
      _._1 match {
        case Application(_, funId :: onlyArg :: Nil) => onlyArg
        case other => other
      }
    )
    assertEquals(expressions.mkString("\n\n"), lambdaNodes.size, expressions.size)
    
    for (expectedExp <- lambdaNodes) {
      val isFound = (false /: expressions) {
        (res, exp) => res || compareNodesModuloVariableName(exp, expectedExp)
      }
      
    	assertTrue(expressions.mkString("\n---\n")  + " do not contain (module variable names) " + expectedExp,
  	    isFound)
    }
  }

}

//object ReconstructorTest {
//  
//  var useEnumerationOrdering: Boolean = _
//  
//  @BeforeClass
//  def saveFlag = {
//    useEnumerationOrdering = Config.useEnumerationOrdering
//    Config.useEnumerationOrdering = false
//  }
//  
//  @AfterClass
//  def restoreFlag = Config.useEnumerationOrdering = useEnumerationOrdering
//  
//}
