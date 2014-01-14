package insynth.common

import insynth.reconstruction.stream._

import org.junit.Assert._

object CommonUtils {
  
  type Output = (Node, Float)
  
  val maxElToOutput = 20
      
  def assertTake(stream: Stream[Output], num: Int) = {
    val result = stream take num
    val message = "Part of the resulting stream: " + result.take(maxElToOutput).mkString("\n")
    
    for (ind <- 0 until result.size - 1)
      assertTrue("Weight are not in non-decreasing order.\n" + "At position " +
      ind + "\n" + message, stream(ind)._2 <= stream(ind + 1)._2)
    result
  }
  
  def compareNodesModuloVariableName(node1: Node, node2: Node): Boolean = {
    def compareNodesModuloVariableNameRec(pair: (Node, Node)): Boolean =
      compareNodesModuloVariableName(pair._1, pair._2)
    
    (node1, node2) match {
      case (Variable(tpe1, _), Variable(tpe2, _)) => tpe1 == tpe2
	  	case (Application(tpe1, params1), Application(tpe2, params2)) =>
	  	  tpe1 == tpe2 && (params1 zip params2).forall( compareNodesModuloVariableNameRec _ )
	  	case (Abstraction(tpe1, vars1, body1), Abstraction(tpe2, vars2, body2)) =>	  	  
	  	  tpe1 == tpe2 && ((vars1 :+ body1) zip (vars2 :+ body2)).forall( compareNodesModuloVariableNameRec _ )
      case _ => node1 == node2
    }
  }
  
  def 

}