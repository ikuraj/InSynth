package insynth.leon

import org.junit.Test
import org.junit.Assert._

import insynth.structures.{ SuccinctType => Type, _ }

import leon.purescala.TypeTrees.{ TypeTree => LeonType, BottomType => LeonBottomType, _ }
import leon.purescala.Definitions.AbstractClassDef
import leon.purescala.Common.FreshIdentifier

class TypeTransformerTest {
  
  val inSynthInteger = Const("Int")
  val inSynthBoolean = Const("Boolean")
  
  val leonBoolean = BooleanType
  
	implicit def singletonList(x: LeonType) = List(x)  
	implicit def listToTSet(x: List[Type]) = TSet(x)  
    
  @Test
  def testTransformer1 { 
    assertEquals(TypeTransformer(leonBoolean), Const("Boolean"))
  }
  
  @Test(expected=classOf[RuntimeException])  
  def testTransformer2 { 
    TypeTransformer(Untyped)
  }
  
  @Test
  def testTransformer3 {
    val leonType: LeonType = 
	    FunctionType(
	      FunctionType ( BooleanType, Int32Type ),
	      FunctionType ( BooleanType, Int32Type )
	    )
    val insynthType: Type =
      Arrow( List ( inSynthBoolean, Arrow( List(inSynthBoolean) , inSynthInteger) ), inSynthInteger )
	    
    assertEquals(TypeTransformer(leonType), insynthType)
  }
  
  @Test
  def testTransformer4 {
    val leonType: LeonType = 
	    TupleType( List ( leonBoolean, leonBoolean ) )
    val insynthType: Type =
      Instance( "Tuple", List (inSynthBoolean, inSynthBoolean) )
	    
    assertEquals(TypeTransformer(leonType), insynthType)
  }
  
  
  @Test
  def testTransformer5 {
    val leonType: LeonType = 
  		AbstractClassType(new AbstractClassDef(FreshIdentifier("$IDontCare$")))
    val insynthType: Type =
      Const("$IDontCare$")
	    
    assertEquals(TypeTransformer(leonType), insynthType)
  }
}