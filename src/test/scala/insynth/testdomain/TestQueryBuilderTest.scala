package insynth.testdomain

import scala.util.Random
import insynth.structures._
import insynth.common._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// enable implicit conversions
import scala.language.implicitConversions

class TestQueryBuilderTest extends AnyFunSuite with Matchers {
  
  import CommonDomainTypes._
  import CommonDeclarations._
  
  val typeBottom = Atom(BottomType)
	  
  test("when goal type is atomic") {
    val queryBuilder = new TestQueryBuilder(typeInt)
    queryBuilder.getQuery.getReturnType should be ( BottomType )
    
    val queryDeclaration = queryBuilder.getQuery.getDeclaration
    
    queryDeclaration.getDomainType should be ( Function(typeInt, typeBottom) )
    queryDeclaration.inSynthType should be ( Arrow(TSet(typeInt.toSuccinctType), BottomType) )
    queryDeclaration.getType should be ( queryDeclaration.inSynthType )
  }
  
}