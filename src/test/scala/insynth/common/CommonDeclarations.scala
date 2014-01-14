package insynth.common

import insynth.structures._

import insynth.testdomain.{ TestQueryBuilder => QueryBuilder, _ }
import insynth.testdomain.{ TestDeclaration => Declaration, _ }


import scala.language.implicitConversions

object CommonDeclarations {
  
  import CommonDomainTypes._

  val booleanDeclaration = TestDeclaration(typeBoolean)  
  val intDeclaration = TestDeclaration(typeInt)      
  val unitDeclaration = TestDeclaration(typeUnit)
  
  val functionBoolToIntDeclaration = TestDeclaration(
    functionBoolToIntType
  )
  
  val functionFun1ToUnitDeclaration = TestDeclaration(
    functionFun1ToUnitType
  )
   
  val functionIntToIntDeclaration = TestDeclaration(
    functionIntToIntType
  )
  
  val threeParFunctionDeclaration = TestDeclaration(     
    threeParFunctionType
  )
  
  val functionFunsToFunDeclaration = TestDeclaration(
    functionFunsToFunType
  )

  val intLeafDeclaration = new TestDeclaration(typeInt)
  intLeafDeclaration.isAbstract_ = true
  val leafIntDeclaration = intLeafDeclaration
  
  val fullNameClassA = "fullNameClassA"
  val objectADeclaration = TestDeclaration(
      objectA, // scala type
      fullNameClassA // full name
  )
  
  object BuildLighterComplexTree {
    import CommonDomainTypes.BuildLighterComplexTree._
        
	  val m1Declaration	= TestDeclaration(
	      m1, "m1"
	  )
	  val m2Declaration = TestDeclaration(
	      m2, "m2"
	  )
  }
  
  object BuildComplexTree {
    import CommonDomainTypes.BuildComplexTree._    

	  val m1Declaration	= TestDeclaration(
	      m1,
	      fullNameClassA + ".m1"
	  )
	  val m2Declaration = TestDeclaration(
	      m2, // inSynth type (implicit conversion)
	      fullNameClassA + ".m2" // full name
	  )
	  val m3Declaration = TestDeclaration(
	      m3,
	      fullNameClassA + ".m3" // full name
      )
	  val m4Declaration = TestDeclaration(
	      m4,
	      fullNameClassA + ".m4" // full name
      )
	  val m5Declaration = TestDeclaration(
	      m5,
	      fullNameClassA + ".m5" // full name
      )
	  val m6Declaration = TestDeclaration(
	      m6,
	      fullNameClassA + ".m6" // full name
      )		
	  // special query declaration
	  val queryDeclaration = TestDeclaration(
	      queryType, 
	      "special.name.for.query"
	    )	  
  }

  object BuildMultipleVarTree {
    import CommonDomainTypes.BuildMultipleVarTree._
        
	  val m1Declaration	= TestDeclaration(m1, "m1")
	  
    val queryBuilder = new QueryBuilder(queryType)
    val queryDeclaration = queryBuilder.getQuery.getDeclaration
  }

  object BuildTreeArrowTypeTree {
    import CommonDomainTypes.BuildTreeArrowTypeTree._

	  val objectADeclaration = Declaration(
      objectA, fullNameClassA
    )
	  
	  val m1Declaration	= Declaration(
      m1, fullNameClassA + ".m1"
	  )

	  val m2Declaration = Declaration(
      m2, fullNameClassA + ".m2"
	  )
	  val m3Declaration = Declaration(
      m3, fullNameClassA + ".m3"
    )	  
	  
	  // special query declaration
	  val queryDeclaration = Declaration(
      queryType, "special.name.for.query"    
    )
	  
	  val outsideDeclaration = Declaration(
      outside, "outside"
    )	 
	  val intValDeclaration = Declaration(
      intVal, "A.intVal"
    )	 
  }

  object FromLeonTestcases {
    def addressBookMakeAddressBookHelpers = {
      val declarationsString =
			  """
				setA : List => Set[Address]
				Nil : Nil
				bComp : Address => Int
				aComp : Address => Int
				size : List => Int
				size : AddressBook => Int
				Cons : (Address, List) => Cons
				priv : Address => Boolean
				= : (Int, Int) => Boolean
				> : (Int, Int) => Boolean
				< : (Int, Int) => Boolean
				<= : (Int, Int) => Boolean
				= : (Boolean, Boolean) => Boolean
				Not : Boolean => Boolean
				And : (Boolean, Boolean) => Boolean
				isInstance[Cons] : List => Boolean
				isInstance[Nil] : List => Boolean
				allPrivate : List => Boolean
				allBusiness : List => Boolean
				addToPers : (AddressBook, Address) => AddressBook
				addToBusiness : (AddressBook, Address) => AddressBook
				AddressBook : (List, List) => AddressBook
				makeAddressBook : List => AddressBook
				a : Cons => Address
				Address : (Int, Int, Boolean) => Address
				l.a : Address
				l.tail : List
				l : Cons
				pers : AddressBook => List
				business : AddressBook => List
				[Nil=>List] : Nil => List
				tail : Cons => List
				[Cons=>List] : Cons => List
			  """.stripMargin
			  
		  val pattern = ".* : (.*)".r
			  
		  for(declLine <- declarationsString.split("\n")) {
		    declLine match {
		      case pattern(asd) => null
		      case _ => null
		    }
		  }
    }
  }

}