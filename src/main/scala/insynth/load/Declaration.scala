package insynth.load

import insynth.structures.{ SuccinctType => Type, _ }
import insynth.structures.Weight._

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