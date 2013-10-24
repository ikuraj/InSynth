package insynth.load

import insynth.structures.{ SuccinctType => Type, _ }
import insynth.structures.Weight._

import insynth.util.FreshNameGenerator

abstract class Declaration(val inSynthType: Type) {        
	
  private var query = false
  
  def getWeight: Weight
  //def setWeight(weight:Weight) { this.weight = weight }
  
  def isQuery = query
  def setQuery(query: Boolean) = {
    this.query = query
		this
  }
      
  def getType = inSynthType
  
  def getSimpleName: String
  
  def getDomainType: DomainType

  def isAbstract: Boolean
  
}

class AbstractDeclaration(override val inSynthType: Type) extends Declaration(inSynthType) {
  
  val nameGenerator = new FreshNameGenerator("#abs#")
  
  // TODO ad-hoc, at least we should allow changing this with some config
  override def getWeight = 1.0f
    
  override def getType = inSynthType
  
  override def getSimpleName = "#abs#"//nameGenerator.getFreshVariableName
  
  override def getDomainType = {
    throw new RuntimeException("I guess straightforward conversion should suffice here")
  }

  override def isAbstract = true
  
}