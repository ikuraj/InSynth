package insynth.testutil

import insynth.leon.{ LeonQueryBuilder => QueryBuilder, _ }
import insynth.reconstruction.intermediate._

import leon.purescala.Definitions.{ FunDef, VarDecl, Program, ObjectDef }
import leon.purescala.Common.{ FreshIdentifier }
import leon.purescala.TypeTrees._
import leon.purescala.Trees.{ Variable => _, _ }

object CommonIntermediate {

  import CommonDeclarations._
  import CommonProofTrees._
    
  def constructBooleanToIntIntermediate = {
    val query = new QueryBuilder(Int32Type)

    val functionApplication =
      Application(
        functionIntToIntType,
        List(
          Set(Identifier(functionBoolToIntType, functionBoolToIntDeclaration)),
          Set(Identifier(BooleanType, booleanDeclaration))))

    val intermediateTree =
      Application(
        query.leonType,
        List(
          Set(Identifier(query.leonType, query.getQuery.getDeclaration)),
          Set(functionApplication)))

    intermediateTree
  }
  
  def constructIntToIntIntermediate = {
    val query = new QueryBuilder(Int32Type)

    val functionApplication =
      Application(
        functionIntToIntType,
        List(
          Set(Identifier(functionIntToIntType, functionIntToIntDeclaration)),
          Set(Identifier(Int32Type, intDeclaration))))

    functionApplication.recursiveParams = List(Set.empty, Set(functionApplication))

    val intermediateTree =
      Application(
        query.leonType,
        List(
          Set(Identifier(query.leonType, query.getQuery.getDeclaration)),
          Set(functionApplication)))

    intermediateTree
  }    
  
  def constructIntToIntIntermediateBool = {
    val query = new QueryBuilder(Int32Type)

    val functionApplicationBoolean =
      Application(
        functionBoolToIntType,
        List(
          Set(Identifier(functionBoolToIntType, functionBoolToIntDeclaration)),
          Set(Identifier(BooleanType, booleanDeclaration))))
          
    val functionApplication =
      Application(
        functionIntToIntType,
        List(
          Set(Identifier(functionIntToIntType, functionIntToIntDeclaration)),
          Set(functionApplicationBoolean)))

    functionApplication.recursiveParams = List(Set.empty, Set(functionApplication))
	
    val intermediateTree =
      Application(
        query.leonType,
        List(
          Set(Identifier(query.leonType, query.getQuery.getDeclaration)),
          Set(functionApplicationBoolean, functionApplication)))

    intermediateTree
  }
  
  def constructIntToIntIntermediateBoth = {
    val query = new QueryBuilder(Int32Type)

    val functionApplicationBoolean =
      Application(
        functionBoolToIntType,
        List(
          Set(Identifier(functionBoolToIntType, functionBoolToIntDeclaration)),
          Set(Identifier(BooleanType, booleanDeclaration))))
          
    val functionApplication =
      Application(
        functionIntToIntType,
        List(
          Set(Identifier(functionIntToIntType, functionIntToIntDeclaration)),
          Set(Identifier(Int32Type, intDeclaration), functionApplicationBoolean)))

    functionApplication.recursiveParams = List(Set.empty, Set(functionApplication))
	
    val intermediateTree =
      Application(
        query.leonType,
        List(
          Set(Identifier(query.leonType, query.getQuery.getDeclaration)),
          Set(functionApplicationBoolean, functionApplication)))

    intermediateTree
  }
  
  // TODO do if we need abstraction (high-order functions)
//  def constructFunctionIntToIntIntermediate = {
//    val query = new QueryBuilder(FunctionType(List(Int32Type), Int32Type))
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
//          Set(Variable(Int32Type, "freshInt"), functionApplicationBoolean)))
//
//    functionApplication.recursiveParams = List(Set(functionApplication))
//	
//		val abstraction = Abstraction(functionIntToIntType,
//	    List(Variable(Int32Type, "var_1")), Set(functionApplicationBoolean, functionApplication))
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