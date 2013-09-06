package insynth
package testdomain

import insynth.structures._
import insynth.structures.Weight._

// need name to differentiate declarations if needed
case class TestDeclaration(
  domainType: DomainType, override val inSynthType: SuccinctType,
  name: String = "testDeclaration", override val weight: Weight = 1.0f,
  var isAbstractFlag: Boolean = false
)
extends insynth.load.Declaration(inSynthType, weight) {
  
  def this(domainType: DomainType, name: String = "testDeclaration", weight: Weight = 1.0f,
    isAbstractFlag: Boolean = false) =
    this(domainType, domainType.toSuccinctType, name, weight) 
  
  override def getSimpleName = name + ":" + domainType.toString// +
  	//"(" + inSynthType.toString + ")" + weight
  
  override def getDomainType = domainType
  
  override def isAbstract = isAbstractFlag
  
}

object TestDeclaration {
  def apply(domainType: DomainType, succinctType: SuccinctType, weight: Weight) =
    new TestDeclaration(domainType, succinctType, "testDeclaration", weight)
  
  def apply(domainType: DomainType, weight: Weight) =
    new TestDeclaration(domainType, "testDeclaration", weight)
  
  def apply(domainType: DomainType, name: String) =
    new TestDeclaration(domainType, name)
  
  def apply(domainType: DomainType) =
    new TestDeclaration(domainType)
    
  def newAbstract(domainTypeOut: DomainType) =
    new TestDeclaration(domainTypeOut, isAbstractFlag = true) 
}