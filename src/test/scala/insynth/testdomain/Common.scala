package insynth.testdomain

import insynth.structures._

object Common {

	val typeInt = Const("Int")
	val typeString = Const("String")
	val typeBoolean = Const("Boolean")
	val typeUnit = Const("Unit")
        
  val booleanDeclaration = TestDeclaration(typeBoolean)  
  val intDeclaration = TestDeclaration(typeInt)      
  val unitDeclaration = TestDeclaration(typeUnit)
  
  val functionBoolToIntType =
    Arrow(TSet(typeBoolean), typeInt)
      
  val functionBoolToIntDeclaration = TestDeclaration(
    functionBoolToIntType
  )
  
  val functionFun1ToUnitType =
    Arrow(TSet(typeUnit, typeInt), typeBoolean)
      
  val functionFun1ToUnitDeclaration = TestDeclaration(
    functionFun1ToUnitType
  )
   
  val functionIntToIntType =
    Arrow(TSet(typeInt), typeInt)
   
  val functionIntToIntDeclaration = TestDeclaration(
    functionIntToIntType
  )

}