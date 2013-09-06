package insynth.common

import insynth.structures._

object CommonDomainTypes {

  val typeInt = Atom(Const("Int"))
  val typeString = Atom(Const("String"))
  val typeBoolean = Atom(Const("Boolean"))
  val typeUnit = Atom(Const("Unit"))
  val typeLong = Atom(Const("Long"))
  val typeChar = Atom(Const("Char"))
  val typeFloat = Atom(Const("Float"))
  val typeDouble = Atom(Const("Double"))
	
  val typeBottom = Atom(Const("$Bottom_Type_Just_For_Resolution$"))

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

}