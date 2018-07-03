package insynth
package testdomain

import insynth.structures._
import insynth.structures.Weight._

// need name to differentiate declarations if needed
case class TestDeclaration(
  domainType: DomainType, override val inSynthType: SuccinctType,
  name: String = "testDeclaration", val weight: Weight = 1.0f  
)
extends insynth.load.Declaration(inSynthType) {
  
  var isAbstract_ = false
  
  def this(domainType: DomainType, name: String, weight: Weight) =
    this(domainType, domainType.toSuccinctType, name, weight) 
 
  def this(domainType: DomainType) =
    this(domainType, domainType.toSuccinctType, "testDeclaration", 1.0f) 

  
  override def getSimpleName = name + ":" + domainType.toString// +
  	//"(" + inSynthType.toString + ")" + weight
  
  override def getDomainType = domainType
  
  override def getWeight = weight
  
  override def isAbstract = isAbstract_
  
  override def toString = name + { if (isAbstract) "#" else "" } + ":" + domainType
}

object TestDeclaration {
  def apply(domainType: DomainType, succinctType: SuccinctType, weight: Weight) =
    new TestDeclaration(domainType, succinctType, "testDeclaration", weight)
  
  def apply(domainType: DomainType, weight: Weight) =
    new TestDeclaration(domainType, "testDeclaration", weight)
  
  def apply(domainType: DomainType, name: String) =
    new TestDeclaration(domainType, name, 1.0f)
  
  def apply(domainType: DomainType) =
    new TestDeclaration(domainType)
}