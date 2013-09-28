package insynth
package testdomain

import insynth.query._
import insynth.structures._
import insynth.engine._

case class TestQueryBuilder(goalType: DomainType) extends QueryBuilder(goalType.toSuccinctType) {
  
  val inSynthRetType = BottomType
  val inSynthType = Arrow(TSet(tpe), inSynthRetType)
  
  val domainRetType = Atom(BottomType)
  val domainType = Function(List(goalType), domainRetType)
  
  def getQuery = TestQuery(inSynthRetType, TestDeclaration(domainType), new InitialSender())
  
}