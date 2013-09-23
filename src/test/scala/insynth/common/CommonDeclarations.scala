package insynth.common

import insynth.structures._

import insynth.testdomain._

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

	  val fullNameClassA = "fullNameClassA"

	  val objectADeclaration = TestDeclaration(
	      objectA, // scala type
	      fullNameClassA // full name
	  )
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

}