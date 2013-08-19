package insynth
package testdomain

import insynth.structures._
import insynth.structures.Weight._

case class TestDeclaration(
  domainType: DomainType,
	override val inSynthType: SuccinctType, override val weight: Weight = 1.0f
)
extends insynth.load.Declaration(inSynthType, weight) {
  
  def this(domainType: DomainType, weight: Weight = 1.0f) = this(domainType, domainType.toSuccinctType, weight) 
  
  override def getSimpleName = inSynthType.toString
  
  override def getDomainType = domainType
  
}

object TestDeclaration {
  def apply(domainType: DomainType, weight: Weight) =
    new TestDeclaration(domainType, weight)
  
  def apply(domainType: DomainType) =
    new TestDeclaration(domainType)
}