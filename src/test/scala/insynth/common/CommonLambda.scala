package insynth.common

import insynth.reconstruction.stream._

import insynth.testdomain.{ TestQueryBuilder => QueryBuilder }

import scala.language.implicitConversions
import scala.language.postfixOps

object CommonLambda {
  implicit def nodeToListNode(node: Node) = List(node) 
  implicit def varToListVar(va: Variable) = List(va) 

  import CommonDeclarations._
  import CommonProofTrees._
  import CommonDomainTypes._
  
  val st = CommonSuccinctTypes
      
  val booleanIdentifier = Identifier(typeBoolean, booleanDeclaration)
  
  def constructBooleanToIntIntermediateLambda = {
    val query = new QueryBuilder(typeInt)

    val functionApplication =
      Application(
        functionBoolToIntType,
        List(
          Identifier(functionBoolToIntType, functionBoolToIntDeclaration),
          booleanIdentifier))

    val intermediateTree =
      Application(
        query.domainType,
        List(
          Identifier(query.domainType, query.getQuery.getDeclaration),
          functionApplication))

    List(intermediateTree)
  }
  
  def constructIntToIntIntermediateFirstLambda(x: Int) = {
    val query = new QueryBuilder(typeInt)

    val functionApplication =
      Application(
        functionIntToIntType,
        List(
          Identifier(functionIntToIntType, functionIntToIntDeclaration),
          Identifier(typeInt, intDeclaration)))

    val (_, listOfApplication) =
      (((Identifier(typeInt, intDeclaration), Nil): (Node, List[Node])) /: (1 to x)) {
      	case ((nextArg, list), _) =>
		      val app =	Application(
		        functionIntToIntType,
		        List(Identifier(functionIntToIntType, functionIntToIntDeclaration),
		          nextArg))
		          
          (app, list :+ app)
    	}
    
    for (app <- listOfApplication) yield 
    	Application(
        query.domainType,
        List(
          Identifier(query.domainType, query.getQuery.getDeclaration),
          app))    
  }    
  
  def constructIntAndBoolToIntIntermediateLambda(x: Int) = {
    val query = new QueryBuilder(typeInt)

    val functionApplicationBoolean =
      Application(
        functionBoolToIntType,
        List(
          Identifier(functionBoolToIntType, functionBoolToIntDeclaration),
          booleanIdentifier))
          
    val functionApplication =
      Application(
        functionIntToIntType,
        List(
          Identifier(functionIntToIntType, functionIntToIntDeclaration),
          Identifier(typeInt, intDeclaration)))

    val (listOfApplication, _) =
      (((Nil, List(Identifier(typeInt, intDeclaration), functionApplicationBoolean)): (List[Node], List[Node])) /: (1 to x)) {
      	case ((list, args), _) =>
		      val listAddition =
		        for (arg <- args) yield
			        Application(functionIntToIntType,
		        		List(Identifier(functionIntToIntType, functionIntToIntDeclaration), arg))
		          
          (list ++ listAddition, listAddition)
    	}
    
    for (app <- listOfApplication) yield 
    	Application(
        query.domainType,
        List(
          Identifier(query.domainType, query.getQuery.getDeclaration),
          app))    
  }  
  
  def constructThreeParFunctionIntermediateLambda(x: Int) = {
    val query = new QueryBuilder(typeInt)

    val listOfApplication =
      ((List(Identifier(typeInt, intDeclaration), Identifier(typeInt, intDeclaration)): List[Node]) /: (1 to x)) {
      	case (list, _) =>
		      val listAddition =
		        (for (arg <- list.combinations(2)) yield
			        Application(
					      threeParFunctionType,
					      List(
					        Identifier(threeParFunctionType, threeParFunctionDeclaration),
					        arg(0),
					        arg(1),
					        booleanIdentifier         
					      )
					    )) ++		      	
		        (for (arg <- list.combinations(2)) yield
			        Application(
					      threeParFunctionType,
					      List(
					        Identifier(threeParFunctionType, threeParFunctionDeclaration),
					        arg(1),
					        arg(0),
					        booleanIdentifier         
					      )
					    ))  ++		      	
		        (for (arg <- list) yield
			        Application(
					      threeParFunctionType,
					      List(
					        Identifier(threeParFunctionType, threeParFunctionDeclaration),
					        arg, arg,
					        booleanIdentifier         
					      )
					    ))				    
		          
          (list ++ listAddition).distinct
    	}
    
    for (app <- listOfApplication.distinct) yield 
    	Application(
        query.domainType,
        List(
          Identifier(query.domainType, query.getQuery.getDeclaration),
          app))
  }
  
  val boolInv = Application(functionBoolToIntType, List(
    Identifier(functionBoolToIntType, functionBoolToIntDeclaration), booleanIdentifier))
  val inv1WithBoolInv = Application(functionIntToIntType, List(
    Identifier(functionIntToIntType, functionIntToIntDeclaration), boolInv))
  val inv1WithInt = Application(functionIntToIntType, List(
    Identifier(functionIntToIntType, functionIntToIntDeclaration), Identifier(typeInt, intDeclaration)))
  val inv2WithInt = Application(functionIntToIntType, List(
    Identifier(functionIntToIntType, functionIntToIntDeclaration), inv1WithInt))
  val inv3WithInt = Application(functionIntToIntType, List(
    Identifier(functionIntToIntType, functionIntToIntDeclaration), inv2WithInt))
  val inv2WithBoolInv = Application(functionIntToIntType, List(
    Identifier(functionIntToIntType, functionIntToIntDeclaration), inv1WithBoolInv))
  val inv3WithBoolInv = Application(functionIntToIntType, List(
    Identifier(functionIntToIntType, functionIntToIntDeclaration), inv2WithBoolInv))
  
  val intIdentifier = Identifier(typeInt, intLeafDeclaration)
  val intVar = Variable(typeInt, "var_1")
    
  object BuildLighterComplexTree {
    import CommonDomainTypes.BuildLighterComplexTree._
    import CommonDeclarations.BuildLighterComplexTree._
    
	  val lambdaNodes = Application(m1, List(
      Identifier(m1, m1Declaration),
      Abstraction(m2, Variable(m2.args.head, "var_1"), Application(m2, List(
        Identifier(m2, m2Declaration),
        intVar))
      ) 
    ))
	}

  object BuildComplexTree {
    import CommonDomainTypes.BuildComplexTree._
    import CommonDeclarations.BuildComplexTree._
    
    val thisIdent = Identifier(objectA, objectADeclaration)
    
	  val lambdaNodes = Iterable(
	      Application(m1, List(
		      Identifier(m1, m1Declaration), 
		      thisIdent,
		      Abstraction(intToString, List(Variable(m2.args(1), "var_2")),
	          Application(m2, List(
			        Identifier(m2, m2Declaration),        
			        thisIdent,
			        intVar)
			      )),
		      Application(m4, List( Identifier(m4, m4Declaration), thisIdent ))
		    )),
	      Application(m1, List(
		      Identifier(m1, m1Declaration), 
		      thisIdent,
		      Abstraction(intToString, List(Variable(typeInt, "var_3")),
	          Application(m6, List(
			        Identifier(m6, m6Declaration),        
			        thisIdent)
			      )),
		      Application(m4, List( Identifier(m4, m4Declaration), thisIdent ))
		    )),
	      Application(m1, List(
		      Identifier(m1, m1Declaration), 
		      thisIdent,
		      Abstraction(intToString, List(Variable(typeInt, "var_2")),
	          Application(m3, List(
			        Identifier(m3, m3Declaration),        
			        thisIdent,
			        Application(m5, List(
				        Identifier(m5, m5Declaration),        
				        thisIdent,
				        intVar
		        		))
			      ))),
		      Application(m4, List( Identifier(m4, m4Declaration), thisIdent ))
		    ))
	    )
	}
  
  object BuildMultipleVarTree {
    import CommonDomainTypes.BuildMultipleVarTree._
    import CommonDeclarations.BuildMultipleVarTree._
    
    val var1 = Variable(typeInt, "var_1")
    val var2 = Variable(typeInt, "var_2")
    
	  val lambdaNodes = Iterable(
      Abstraction(queryType, List(var1),
    		Abstraction(queryType.retType, List(var2),
  				Application(m1, List(Identifier(m1, m1Declaration), var1))
    		)
  		)
    )
	}
  
  object BuildArrowTypeTree {
    import CommonDomainTypes.BuildTreeArrowTypeTree._
    import CommonDeclarations.BuildTreeArrowTypeTree._
    
	  //  expression: 
	  //	(Int,Int) -> m1(this)(_,_)
    
    val var1 = Variable(typeInt, "var_1")
    val var2 = Variable(typeInt, "var_2")
    
    val thisIdent = Identifier(objectA, objectADeclaration)
    val intValA = Identifier(typeInt, intValDeclaration)

    def makeM1Application(n1: Node, n2: Node) =
  		Abstraction(intIntToChar, List(var1, var2),
		    Application(intIntToChar, List(
		    	// NOTE its not the same function type since its return type is used in application
	    		Application(m1, List(Identifier(m1, m1Declaration), thisIdent)),
	    		n1, n2
	    		)
				)
  		)
    
	  val lambdaNodes = Iterable(
  		makeM1Application(var1, var2),
  		makeM1Application(var1, intValA),
  		makeM1Application(intValA, var1),
  		makeM1Application(intValA, intValA)
    )
	}
  
  object BuildArrowTypeTreeMoreComplex {
    import BuildArrowTypeTree._
    import CommonDomainTypes.BuildTreeArrowTypeTree._
    import CommonDeclarations.BuildTreeArrowTypeTree._
    
	  //  expression: 
	  //	(Int,Int) -> m1(this)(_,_) | (Int,Int) -> m1(this)(intVal, intVal)
	  //	(Int,Int) -> m2(this,_,_) | m2(this, intVal, intVal)
	  //	(Int,Int) -> m3(this) | outside
    
    def makeM2Application(n1: Node, n2: Node) =
  		Abstraction(intIntToChar, List(var1, var2),
		    Application(m2, List(
		    	// NOTE its not the same function type since its return type is used in application
	    		Identifier(m2, m2Declaration), thisIdent, n1, n2
    		))
  		)

    def makeM3Application =
  		Abstraction(intIntToChar, List(var1, var2),
    		Application(m3, List(Identifier(m3, m3Declaration), thisIdent))
  		)

    def makeOutsideApplication(n1: Node, n2: Node) =
  		Abstraction(intIntToChar, List(var1, var2),
		    Application(outside, List(
	    		Identifier(outside, outsideDeclaration), n1, n2
				))
  		)
    
	  val lambdaNodes =
      (for (n1 <- List(var1, var2, intValA); n2 <- List(var1, var2, intValA)) yield {
        makeM1Application(n1, n2) :: makeM2Application(n1, n2) :: makeOutsideApplication(n1, n2)
      }).flatten.distinct :+ makeM3Application
	}
    
  // TODO do if we need abstraction (high-order functions)
//  def constructFunctionIntToIntIntermediateLambda = {
//    val query = new QueryBuilder(FunctionType(List(Int32Type), Int32Type))
//
//    val functionApplicationBoolean =
//      Application(
//        functionBoolToIntType,
//        List(
//          Set(Identifier(functionIntToIntType, functionBoolToIntDeclaration)),
//          Set(booleanIdentifier)))
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