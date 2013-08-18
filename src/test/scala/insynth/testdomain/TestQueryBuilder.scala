package insynth
package testdomain

import insynth.query._
import insynth.structures._
import insynth.engine._

case class TestQueryBuilder(goalType: SuccinctType) extends QueryBuilder(goalType) {
  
  val inSynthRetType = BottomType
  val inSynthType = Arrow(TSet(tpe), inSynthRetType)
  
  def getQuery = TestQuery(inSynthRetType, new TestDeclaration(inSynthType), new InitialSender())
  
}