package insynth.common

import insynth.structures._

import scala.language.implicitConversions

object CommonDomainTypes {
  implicit def typeToList(typ: DomainType) = List(typ)

	val typeInt = Atom(Const("Int"))
	val typeString = Atom(Const("String"))
	val typeBoolean = Atom(Const("Boolean"))
	val typeUnit = Atom(Const("Unit"))
	val typeChar = Atom(Const("Char"))
	val typeLong = Atom(Const("Long"))

  // class A { ... }
	val typeObjectA = Atom(Const("A"))
  val objectA = typeObjectA

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

  object BuildComplexTree {	  
	  // def m1(f: Int=>String, c:Char): Boolean
	  val m1 = Function(
	      List ( objectA, Function(typeInt, typeString), typeChar ), // parameters
	      typeBoolean // return type
		)	
	  // def m2(a: Int): String 
	  val m2 = Function(List(objectA, typeInt), typeString)
	  // def m3(a:Long): String
	  val m3 = Function(List(objectA, typeLong), typeString)
	  // def m4(): Char
	  val m4 = Function(List(objectA), typeChar)
	  // def m5(a: Int): Long
	  val m5 = Function(List(objectA, typeInt), typeLong)
	  // def m6(): String
	  val m6 = Function(List(objectA), typeString)
	  // query: typeBoolean → ⊥
	  val queryType = Function(typeBoolean, typeBottom)

	  val intToString = Function(typeInt, typeString)
	}

  object BuildMultipleVarTree {	  
	  val m1 = Function( typeInt, typeString )

	  val queryType = Function(typeInt, Function(typeInt, typeString))
	}

  object BuildTreeArrowTypeTree {	  
    val intIntToChar = Function(List(typeInt, typeInt), typeChar)

	  // def m1(): ((Int, Int)=>Char)
	  val m1 = Function(
      List(objectA), // parameters
      intIntToChar // return type
		)	
	  // def m2(a: Int, b:Int): Char
	  val m2 = Function(List(objectA, typeInt, typeInt), typeChar)
	  // def m3(): Char
	  val m3 = Function(List(objectA), typeChar)
	  // query: String → ⊥
	  val queryType = Function(intIntToChar, typeBottom)
	  // def outside(a: Int, b:Int): Char
	  val outside = intIntToChar
	  // val intVal: Int
	  val intVal = typeInt
	}
    
}