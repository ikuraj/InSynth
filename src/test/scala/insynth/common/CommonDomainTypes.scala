package insynth.common

import insynth.structures._

object CommonDomainTypes {
  implicit def typeToList(typ: DomainType) = List(typ)

	val typeInt = Atom(Const("Int"))
	val typeString = Atom(Const("String"))
	val typeBoolean = Atom(Const("Boolean"))
	val typeUnit = Atom(Const("Unit"))
	val typeChar = Atom(Const("Char"))
	val typeLong = Atom(Const("Long"))

	val typeObjectA = Atom(Const("A"))

  val typeBottom = Atom(BottomType)
                
  val functionBoolToIntType =
    Function(List(typeBoolean), typeInt)
  
  val functionFun1ToUnitType =
    Function(List(typeUnit, typeInt), typeBoolean)
   
  val functionIntToIntType =
    Function(List(typeInt), typeInt)
    
  val threeParFunctionType =
    Function(List(typeInt, typeInt, typeBoolean), typeInt)

  val functionFunsToFunType =
    Function(List(functionBoolToIntType, functionFun1ToUnitType), functionIntToIntType)

  object BuildLighterComplexTree {	  
	  val m1 = Function(
	      List (Function(typeInt, typeString)), // parameters
	      typeBoolean // return type
		)	
	  val m2 = Function(typeInt, typeString)
	}
    
}