package insynth.common

import insynth.reconstruction.intermediate._

import insynth.testdomain.{ TestQueryBuilder => QueryBuilder }
import insynth.common._

object CommonIntermediate {

  import CommonDeclarations._
  import CommonProofTrees._
  import CommonDomainTypes._
  
  val st = CommonSuccinctTypes
    
  def constructBooleanToIntIntermediate = {
    val query = new QueryBuilder(st.typeInt)

    val functionApplication =
      Application(
        functionBoolToIntType,
        List(
          Set(Identifier(functionBoolToIntType, functionBoolToIntDeclaration)),
          Set(Identifier(typeBoolean, booleanDeclaration))))

    val intermediateTree =
      Application(
        query.domainType,
        List(
          Set(Identifier(query.domainType, query.getQuery.getDeclaration)),
          Set(functionApplication)))

    intermediateTree
  }
  
  def constructIntToIntIntermediate = {
    val query = new QueryBuilder(st.typeInt)

    val functionApplication =
      Application(
        functionIntToIntType,
        List(
          Set(Identifier(functionIntToIntType, functionIntToIntDeclaration)),
          Set(Identifier(typeInt, intDeclaration))))

    functionApplication.recursiveParams = List(Set.empty, Set(functionApplication))

    val intermediateTree =
      Application(
        query.domainType,
        List(
          Set(Identifier(query.domainType, query.getQuery.getDeclaration)),
          Set(functionApplication)))

    intermediateTree
  }    
  
  def constructIntToIntIntermediateBool = {
    val query = new QueryBuilder(st.typeInt)

    val functionApplicationBoolean =
      Application(
        functionBoolToIntType,
        List(
          Set(Identifier(functionBoolToIntType, functionBoolToIntDeclaration)),
          Set(Identifier(typeBoolean, booleanDeclaration))))
          
    val functionApplication =
      Application(
        functionIntToIntType,
        List(
          Set(Identifier(functionIntToIntType, functionIntToIntDeclaration)),
          Set(functionApplicationBoolean)))

    functionApplication.recursiveParams = List(Set.empty, Set(functionApplication))
	
    val intermediateTree =
      Application(
        query.domainType,
        List(
          Set(Identifier(query.domainType, query.getQuery.getDeclaration)),
          Set(functionApplicationBoolean, functionApplication)))

    intermediateTree
  }
  
  def constructIntToIntIntermediateBoth = {
    val query = new QueryBuilder(st.typeInt)

    val functionApplicationBoolean =
      Application(
        functionBoolToIntType,
        List(
          Set(Identifier(functionBoolToIntType, functionBoolToIntDeclaration)),
          Set(Identifier(typeBoolean, booleanDeclaration))))
          
    val functionApplication =
      Application(
        functionIntToIntType,
        List(
          Set(Identifier(functionIntToIntType, functionIntToIntDeclaration)),
          Set(Identifier(typeInt, intDeclaration), functionApplicationBoolean)))

    functionApplication.recursiveParams = List(Set.empty, Set(functionApplication))
	
    val intermediateTree =
      Application(
        query.domainType,
        List(
          Set(Identifier(query.domainType, query.getQuery.getDeclaration)),
          Set(functionApplicationBoolean, functionApplication)))

    intermediateTree
  }
  
  // TODO do if we need abstraction (high-order functions)
//  def constructFunctionIntToIntIntermediate = {
//    val query = new QueryBuilder(FunctionType(List(typeInt), typeInt))
//
//    val functionApplicationBoolean =
//      Application(
//        functionBoolToIntType,
//        List(
//          Set(Identifier(functionIntToIntType, functionBoolToIntDeclaration)),
//          Set(Identifier(BooleanType, booleanDeclaration))))
//          
//    val functionApplication =
//      Application(
//        functionIntToIntType,
//        List(
//          Set(Identifier(functionIntToIntType, functionIntToIntDeclaration)),
//          Set(Variable(typeInt, "freshInt"), functionApplicationBoolean)))
//
//    functionApplication.recursiveParams = List(Set(functionApplication))
//	
//		val abstraction = Abstraction(functionIntToIntType,
//	    List(Variable(typeInt, "var_1")), Set(functionApplicationBoolean, functionApplication))
//
//    val intermediateTree =
//      Application(
//        query.leonType,
//        List(
//          Set(Identifier(query.leonType, query.getQuery.getDeclaration)),
//          Set(abstraction)))
//
//    intermediateTree
//  }

}