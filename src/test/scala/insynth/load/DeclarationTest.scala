package insynth.load

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import insynth.testdomain._
import insynth.common._

class DeclarationTest extends FunSuite with ShouldMatchers {
  
  import CommonDeclarations._
  val dt = CommonDomainTypes
  val st = CommonSuccinctTypes
  
  test("declarations build from domain types and corresponding sucinct types should match") {
  	booleanDeclaration should equal(TestDeclaration(dt.typeBoolean, st.typeBoolean))
  	intDeclaration should equal(TestDeclaration(dt.typeInt, st.typeInt))   
  	unitDeclaration should equal(TestDeclaration(dt.typeUnit, st.typeUnit))
  
  	functionBoolToIntDeclaration should equal(TestDeclaration(dt.functionBoolToIntType, st.functionBoolToIntType))
  	functionFun1ToUnitDeclaration should equal(TestDeclaration(dt.functionFun1ToUnitType, st.functionFun1ToUnitType))
  	functionIntToIntDeclaration should equal(TestDeclaration(dt.functionIntToIntType, st.functionIntToIntType))
  	
  	functionFunsToFunDeclaration should equal(TestDeclaration(dt.functionFunsToFunType, st.functionFunsToFunType))
  }
  
  test("declarations should have default weight 1.0f") {
  	booleanDeclaration should equal(TestDeclaration(dt.typeBoolean, st.typeBoolean, 1f))
  }
  
}